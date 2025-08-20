#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2025 Qiqi Jin
#
# Author:     Qiqi Jin <ginqi7@gmail.com>
# Maintainer: Qiqi Jin <ginqi7@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os
from core.buffer import Buffer
from core.utils import interactive, message_to_emacs, eval_in_emacs, set_emacs_var
from PyQt6 import QtCore, QtWidgets
from PyQt6.QtCore import QEvent, QRectF, QSizeF, Qt, QUrl
from PyQt6.QtGui import QBrush, QColor, QPainter
from PyQt6.QtMultimedia import QAudioOutput, QMediaPlayer
from PyQt6.QtMultimediaWidgets import QGraphicsVideoItem
from PyQt6.QtWidgets import QGraphicsScene, QGraphicsView, QHBoxLayout, QVBoxLayout, QWidget
# hack: add current dir path to sys.path for relative path import other modules.
import sys

sys.path.append(os.path.dirname(__file__))
from elements import EditElements
from ffmpeg_utils import get_keyframe_timestamps,  get_video_info, get_video_resolution, export_elements_to_streams, export_streams


class AppBuffer(Buffer):
    def __init__(self, buffer_id, url, arguments):
        Buffer.__init__(self, buffer_id, url, arguments, True)

        self.background_color = QColor(0, 0, 0)

        self.add_widget(VideoPlayer(self.theme_background_color, self.theme_foreground_color))
        self.buffer_widget.play(url)

        self.build_all_methods(self.buffer_widget)

    def all_views_hide(self):
        # Pause video before all views hdie, otherwise will got error "Internal data stream error".
        if self.buffer_widget.media_player.playbackState() == QMediaPlayer.PlaybackState.PlayingState:
            self.buffer_widget.media_player.pause()
            self.buffer_widget.video_need_replay = True

    def some_view_show(self):
        if self.buffer_widget.video_need_replay is True:
            self.buffer_widget.media_player.play()

    def save_session_data(self):
        return str(self.buffer_widget.media_player.position())

    def restore_session_data(self, session_data):
        position = int(session_data)
        self.buffer_widget.media_player.setPosition(position)

    def toggle_play(self):
        if self.buffer_widget.media_player.playbackState() == QMediaPlayer.PlaybackState.PlayingState:
            self.buffer_widget.media_player.pause()
            self.buffer_widget.video_need_replay = False
        else:
            self.buffer_widget.media_player.play()
            self.buffer_widget.video_need_replay = True

    def destroy_buffer(self):
        self.buffer_widget.media_player.pause()

        super().destroy_buffer()

class VideoPlayer(QWidget):

    def __init__(self, theme_background_color, theme_foreground_color):
        super(VideoPlayer, self).__init__()

        self.scene = QGraphicsScene(self)
        self.scene.setBackgroundBrush(QBrush(QColor(0, 0, 0, 255)))

        self.graphics_view = QGraphicsView(self.scene)
        self.graphics_view.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.graphics_view.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.graphics_view.setFrameStyle(0)
        self.graphics_view.setStyleSheet("QGraphicsView {background: transparent; border: 3px; outline: none;}")

        self.is_button_press = False

        self.video_item = QGraphicsVideoItem()

        self.panel_height = 60
        self.progress_bar_height = 60
        self.panel_padding_x = 0
        self.panel_padding_y = (self.panel_height - self.progress_bar_height) / 2

        self.control_panel_widget = QWidget()
        self.control_panel_widget.setStyleSheet("background-color: transparent;")
        self.progress_bar_layout = QHBoxLayout(self.control_panel_widget)
        self.progress_bar_layout.setContentsMargins(
            int(self.panel_padding_x),
            int(self.panel_padding_y),
            int(self.panel_padding_x),
            int(self.panel_padding_x))

        self.control_panel = ControlPanel()

        self.progress_bar = ProgressBar(theme_background_color, theme_foreground_color)
        self.progress_bar.progress_changed.connect(self.update_video_progress)
        self.progress_bar_layout.addWidget(self.progress_bar)

        self.layout = QVBoxLayout(self)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.graphics_view)

        self.scene.addItem(self.video_item)
        self.scene.addItem(self.control_panel)
        self.control_panel_proxy_widget = self.scene.addWidget(self.control_panel_widget)

        self.media_player = QMediaPlayer()
        self.audio_output = QAudioOutput()

        self.media_player.positionChanged.connect(self.progress_change)
        self.media_player.setVideoOutput(self.video_item)
        self.media_player.setAudioOutput(self.audio_output)

        self.video_need_replay = False
        self.video_seek_durcation = 10000 # in milliseconds

        self.video_info = None
        self.video_resolution = None
        self.edit_elements : EditElements = EditElements()
        self.is_play_only_in_clips = False
        self.loop_in_clip = None
        self.url = None

        self.graphics_view.viewport().installEventFilter(self)

    def get_current_clip(self, position):
        for clip in self.edit_elements.get_clips():
            if clip.end and position >= clip.begin and position <= clip.end:
                return clip

    def get_next_clip(self, position):
        for clip in self.edit_elements.get_clips():
            if position < clip.begin:
                return clip

    def update_video_progress(self, percent):
        self.media_player.setPosition(int(self.media_player.duration() * percent))

    def progress_change(self, position):
        self.progress_bar.update_progress(self.media_player.duration(), position)
        if self.loop_in_clip:
            if position < self.loop_in_clip[0] or position > self.loop_in_clip[1]:
                self.media_player.setPosition(self.loop_in_clip[0])

        if self.is_play_only_in_clips and not self.get_current_clip(position):
            clip = self.get_next_clip(position)
            target = self.media_player.duration()
            if clip:
                target = clip.begin
            else:
                first_clip = self.edit_elements.first_clip()
                if first_clip:
                    target = first_clip.begin

            self.media_player.setPosition(target)


    def resizeEvent(self, event):
        self.video_item.setSize(QSizeF(event.size().width(), event.size().height()))

        self.control_panel.update_size(event.size().width(), self.panel_height)
        self.control_panel.setPos(0, event.size().height() - self.panel_height)

        self.control_panel_widget.resize(event.size().width(), self.panel_height)
        self.control_panel_proxy_widget.setPos(0, event.size().height() - self.panel_height)

        self.progress_bar.resize(event.size().width() - self.panel_padding_x * 2, self.progress_bar_height)

        QWidget.resizeEvent(self, event)

    def play(self, url):
        self.url = url
        self.media_player.setSource(QUrl.fromLocalFile(url))
        self.media_player.play()
        self.video_info = get_video_info(url)
        self.video_resolution = get_video_resolution(self.video_info)
        keyframe_timestamps = get_keyframe_timestamps(self.video_info)
        self.progress_bar.keyframes = keyframe_timestamps
        set_emacs_var("eaf-video-editor--org-file", url+".org")

    def generate_copy_url(self, original_url):
        '''
        Generate a new URL based on the provided file URL, with the filename changed to copy_original_filename.
        Parameters:
          - original_url (str): The original file URL

        Returns:
          - str: The newly generated URL
        '''
        directory, filename = os.path.split(original_url)
        name, ext = os.path.splitext(filename)
        new_filename = f"copy_{name}{ext}"
        new_url = os.path.join(directory, new_filename)
        return new_url

    def eventFilter(self, obj, event):
        if event.type() in [QEvent.Type.MouseButtonPress]:
            self.is_button_press = True
            self.loop_in_clip = None
            self.media_player.pause()
        elif event.type() in [QEvent.Type.MouseButtonRelease]:
            self.is_button_press = False

        return False

    def hide_control_panel(self):
        self.control_panel.hide()
        self.control_panel_proxy_widget.hide()

    def show_control_panel(self):
        self.control_panel.show()
        self.control_panel_proxy_widget.show()

    @interactive
    def play_forward(self):
        video_position = self.media_player.position()
        self.media_player.setPosition(video_position + self.video_seek_durcation)
        message_to_emacs("Forward to: {}%".format(self.media_player.position() / self.media_player.duration() * 100))

    @interactive
    def play_backward(self):
        video_position = self.media_player.position()
        self.media_player.setPosition(max(video_position - self.video_seek_durcation, 0))
        message_to_emacs("Forward to: {}%".format(self.media_player.position() / self.media_player.duration() * 100))

    @interactive
    def increase_volume(self):
        self.audio_output.setVolume(self.audio_output.volume() + 0.1)
        message_to_emacs("Increase volume to: {}%".format(self.audio_output.volume() * 100))

    @interactive
    def decrease_volume(self):
        self.audio_output.setVolume(self.audio_output.volume() - 0.1)
        message_to_emacs("Decrease volume to: {}%".format(self.audio_output.volume() * 100))

    @interactive
    def restart(self):
        self.media_player.setPosition(0)

    @interactive
    def clip_point(self):
        position = self.media_player.position()
        last_clip = self.edit_elements.last_clip()
        if not last_clip or last_clip.end:
            self.edit_elements.add_clip(self.url, position, None)
            message_to_emacs(f'Add Clip Begin: {position}')
        if last_clip and not last_clip.end:
            last_clip.end = position
            eval_in_emacs("eaf-video-editor--add-clip", last_clip.to_simple())
            message_to_emacs(f'Add Clip: {last_clip.range_str()}')

        self.progress_bar.clips = self.edit_elements.get_clips()
        self.progress_bar.update()

    @interactive
    def toggle_play_clips(self):
        self.is_play_only_in_clips = not self.is_play_only_in_clips
        message_to_emacs(f'Only Play Clips: {self.is_play_only_in_clips}')


    @interactive
    def loop_play_in_clip(self, clip_begin, clip_end):
        self.loop_in_clip = (clip_begin, clip_end)

    def update_clips(self, clips):
        self.progress_bar.clips = clips

    @interactive
    def update_edit_elements(self, elements):
        self.edit_elements.from_emacs(self.url, elements)
        self.update_clips(self.edit_elements.get_clips())


    @interactive
    def export(self):
        if not self.url:
            return
        message_to_emacs("Start export ...")
        streams = export_elements_to_streams(self.edit_elements.edit_elements, self.url, self.video_resolution)
        output = self.generate_copy_url(self.url)
        export_streams(output, streams)
        # convert_clips_to_video(self.url, output, self.clips)
        message_to_emacs(f"Successfully generated a new video.: {output}")

class ControlPanel(QtWidgets.QGraphicsItem):
    def __init__(self, parent=None):
        super(ControlPanel, self).__init__(parent)
        self.height = 0
        self.width = 0
        self.background_color = QColor(0, 0, 0, 255)
        self.setOpacity(0.9)

    def update_size(self, width, height):
        self.width = width
        self.height = height
        self.update()

    def paint(self, painter, option, widget):
        painter.setPen(self.background_color)
        painter.setBrush(self.background_color)
        painter.drawRect(0, 0, self.width, self.height)

    def boundingRect(self):
        return QRectF(0, 0, self.width, self.height)

class ProgressBar(QWidget):

    progress_changed = QtCore.pyqtSignal(float)

    def __init__(self, theme_background_color, theme_foreground_color):
        super(QWidget, self).__init__()
        self.foreground_color = QColor(theme_foreground_color)
        self.background_color = QColor(theme_background_color)
        self.position = 0
        self.duration = 0
        self.is_press = False
        self.render_height = 40
        self.keyframes = []
        self.clips = []

    def update_progress(self, duration, position):
        self.position = position
        self.duration = duration

        self.update()

    def mousePressEvent(self, event):
        self.is_press = True
        self.progress_changed.emit(event.position().x() * 1.0 / self.width())

    def mouseReleaseEvent(self, event):
        self.is_press = False

    def mouseMoveEvent(self, event):
        if self.is_press:
            self.progress_changed.emit(event.position().x() * 1.0 / self.width())

    def paintBar(self, painter):
        render_y = int((self.height() - self.render_height) / 2)

        painter.setPen(self.background_color)
        painter.setBrush(self.background_color)

        painter.drawRect(0, render_y, int(self.width()), int(self.render_height))

    def paintKeyFrameLines(self, painter):
        render_y = int((self.height() - self.render_height) / 2)

        for frame in self.keyframes:
            painter.setPen(Qt.GlobalColor.gray)
            painter.setBrush(QBrush(Qt.GlobalColor.gray))
            x = int(self.width() * frame / self.duration)
            painter.drawLine(x, render_y, x, render_y + int(self.render_height))

    def paintClips(self, painter):
        render_y = int((self.height() - self.render_height) / 2)
        for clip in self.clips:
            if clip.end:
                painter.setPen(Qt.GlobalColor.gray)
                painter.setBrush(QBrush(Qt.GlobalColor.gray))
                x1 = int(self.width() * clip.begin / self.duration)
                x2 = int(self.width() * clip.end / self.duration)
                painter.drawRect(x1, render_y, x2-x1, int(self.render_height))
            else:
                painter.setPen(Qt.GlobalColor.green)
                painter.setBrush(QBrush(Qt.GlobalColor.green))
                x = int(self.width() * clip.begin / self.duration)
                painter.drawLine(x, render_y, x, render_y + int(self.render_height))

    def paintCurrentLine(self, painter):
        render_y = int((self.height() - self.render_height) / 2)
        painter.setPen(Qt.GlobalColor.red)
        painter.setBrush(QBrush(Qt.GlobalColor.red))
        x = int(self.width() * self.position / self.duration)
        painter.drawLine(x, render_y, x, render_y + int(self.render_height))


    def paintEvent(self, event):
        painter = QPainter(self)
        self.paintBar(painter)

        if self.duration > 0:
            self.paintKeyFrameLines(painter)
            self.paintClips(painter)
            self.paintCurrentLine(painter)
