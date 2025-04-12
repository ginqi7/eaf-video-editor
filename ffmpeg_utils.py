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

import ffmpeg
import os

def image_to_stream(image):
        return ffmpeg.input(image, loop=1, t=10, framerate=30)

def text_to_stream(text, time, resolution):
        width, height = resolution
        return ffmpeg.input(f"color=c=white:s={width}x{height}:r=30,drawtext=text='{text}':fontcolor=black:fontsize=50:x=(w-tw)/2:y=(h-th)/2",
                            f="lavfi",
                            t= time)

def export_elements_to_streams(elements, input, resolution):
        return [export_element_to_stream(element, input, resolution) for element in elements]

def export_element_to_stream(element, input, resolution):
        print(element)
        if element[0] == "eaf-ve-clip":
                return clips_to_video(input, element[1])
        elif element[0] == "eaf-ve-text":
                return text_to_stream(element[1][0], element[1][1], resolution)


def clips_to_video(input, clip):
        start_time, end_time = clip[0] / 1000, clip[1] / 1000
        return ffmpeg.input(input, ss=start_time, t=end_time - start_time)

def export_streams(output_path, streams):
        merged_stream = ffmpeg.concat(*streams)
        # Output to new video file
        # , v=1# , a=1
        # v=1 indicates a video stream, and a=1 indicates an audio stream.
        if os.path.exists(output_path):
                os.remove(output_path)
        output = ffmpeg.output(merged_stream, output_path)
        ffmpeg.run(output)

def get_video_info(video_path):
        try:
                return ffmpeg.probe(video_path, show_frames=None, select_streams="v", skip_frame="nokey", of="json")
        except Exception as e:
                print(e.stderr)


def get_keyframe_timestamps(info):
        frames = info['frames']
        keyframes = [frame for frame in frames if frame.get('key_frame') == 1]
        keyframe_timestamps = [float(frame['pts_time']) * 1000 for frame in keyframes if 'pts_time' in frame]
        # print(keyframe_timestamps)
        return keyframe_timestamps

def get_video_resolution(info):
        video_stream = next((stream for stream in info['streams'] if stream['codec_type'] == 'video'), None)
        if video_stream is None:
            print("Error: No video stream found in the file")
            return None
        width = video_stream['width']
        height = video_stream['height']

        print(f"Resolution: {width}x{height}")
        return (width, height)
