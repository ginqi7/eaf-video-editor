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
import elements


def image_to_stream(file: str, time: int, resolution: list[int]):
    width, height = resolution
    video_stream = ffmpeg.input(file, loop=1, t=time, framerate=30).video
    # 2. Create a silence audio stream of equal length.
    audio_stream = ffmpeg.input("anullsrc", f="lavfi", t=time).audio
    return video_stream.filter("scale", width, height), audio_stream


def text_to_stream(text: str, time: int, resolution: list[int]):
    width, height = resolution

    video_stream = ffmpeg.input(
        f"color=c=white:s={width}x{height}:r=30,drawtext=text='{text}':fontcolor=black:fontsize=50:x=(w-tw)/2:y=(h-th)/2",
        f="lavfi",
        t=time,
    ).video
    audio_stream = ffmpeg.input("anullsrc", f="lavfi", t=time).audio
    return video_stream, audio_stream


def export_elements_to_streams(
    elements: list[elements.EditElement], input: str, resolution: list[int]
):
    return [
        export_element_to_stream(element, input, resolution) for element in elements
    ]


def export_element_to_stream(element, input, resolution):
    if isinstance(element, elements.EditClip):
        return clips_to_video(input, element)
    elif isinstance(element, elements.EditText):
        return text_to_stream(element.text, element.duration, resolution)
    elif isinstance(element, elements.EditImage):
        return image_to_stream(element.file, element.duration, resolution)


def clips_to_video(input, clip: elements.EditClip):
    start_time, end_time = clip.begin / 1000, clip.end / 1000
    print(f"clip: {start_time} -> {end_time}")
    stream = ffmpeg.input(input, ss=start_time, t=end_time - start_time)
    video_stream = stream.video
    audio_stream = stream.audio
    for mute in clip.mutes:
        mute_begin = mute.begin / 1000 - start_time
        mute_end = mute.end / 1000 - start_time
        audio_stream = audio_stream.filter(
            "volume", enable=f"between(t,{mute_begin},{mute_end})", volume=0
        )
    return video_stream, audio_stream


def export_streams(output_path, streams):
    expanded_streams = []
    for stream in streams:
        if stream:
            video_stream, audio_stream = stream
            expanded_streams.append(video_stream)
            expanded_streams.append(audio_stream)
    merged_stream = ffmpeg.concat(*expanded_streams, v=1, a=1)
    # Output to new video file
    # , v=1# , a=1
    # v=1 indicates a video stream, and a=1 indicates an audio stream.
    if os.path.exists(output_path):
        os.remove(output_path)
    output = ffmpeg.output(merged_stream, output_path, vcodec="libx264", acodec="aac")
    ffmpeg.run(output)


def get_video_info(video_path):
    try:
        return ffmpeg.probe(
            video_path,
            show_frames=None,
            select_streams="v",
            skip_frame="nokey",
            of="json",
        )
    except Exception as e:
        print(e.stderr)


def get_keyframe_timestamps(info):
    frames = info["frames"]
    keyframes = [frame for frame in frames if frame.get("key_frame") == 1]
    keyframe_timestamps = [
        float(frame["pts_time"]) * 1000 for frame in keyframes if "pts_time" in frame
    ]
    # print(keyframe_timestamps)
    return keyframe_timestamps


def get_video_resolution(info):
    video_stream = next(
        (stream for stream in info["streams"] if stream["codec_type"] == "video"), None
    )
    if video_stream is None:
        print("Error: No video stream found in the file")
        return None
    width = video_stream["width"]
    height = video_stream["height"]

    print(f"Resolution: {width}x{height}")
    return (width, height)
