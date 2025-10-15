from enum import Enum, unique


@unique
class EditElementType(Enum):
    CLIP = "eve-clip"
    TEXT = "eve-text"
    IMAGE = "eve-image"
    MUTE = "eve-mute"


class EditElement:
    def __init__(self, type: EditElementType):
        self.type: EditElementType = type


class EditMute(EditElement):
    def __init__(self, clip, begin, end):
        EditElement.__init__(self, EditElementType.MUTE)
        self.clip = clip
        self.begin = begin
        self.end = end
        self.clip.mutes.append(self)

    def range_str(self):
        return f"[{self.begin} -> {self.end}]"

    def to_simple(self):
        return [self.begin, self.end]


class EditClip(EditElement):
    def __init__(self, file, begin, end):
        EditElement.__init__(self, EditElementType.CLIP)
        self.file = file
        self.begin = begin
        self.end = end
        self.mutes = []

    def add_mute(self, mute):
        self.mutes.append(mute)

    def last_mute(self):
        if self.mutes:
            return self.mutes[-1]

    def range_str(self):
        return f"[{self.begin} -> {self.end}]"

    def to_simple(self):
        return [self.begin, self.end]


class EditText(EditElement):
    def __init__(self, text: str, duration: int):
        EditElement.__init__(self, EditElementType.TEXT)
        self.text: str = text
        self.duration: int = duration


class EditImage(EditElement):
    def __init__(self, file: str, duration: int):
        EditElement.__init__(self, EditElementType.IMAGE)
        self.file: str = file
        self.duration: int = duration


class EditElements:
    def __init__(self):
        self.edit_elements: list[EditElement] = []

    def get_by_type(self, type: EditElementType):
        return [x for x in self.edit_elements if x.type == type]

    def get_clips(self):
        return self.get_by_type(EditElementType.CLIP)

    def get_texts(self):
        return self.get_by_type(EditElementType.TEXT)

    def get_mutes(self):
        return self.get_by_type(EditElementType.MUTE)

    def add_clip(self, file: str, begin: int, end: int):
        self.edit_elements.append(EditClip(file, begin, end))

    def add_text(self, text: str, duration: int):
        self.edit_elements.append(EditText(text, duration))

    def add_image(self, file: str, duration: int):
        self.edit_elements.append(EditImage(file, duration))

    def add_mute(self, clip: EditClip, begin: int, end: int):
        self.edit_elements.append(EditMute(clip, begin, end))

    def last_clip(self):
        clips = self.get_clips()
        if clips:
            return clips[-1]

    def first_clip(self):
        clips = self.get_clips()
        if clips:
            return clips[0]

    def search_clip(self, time):
        return next(
            (
                clip
                for clip in self.get_clips()
                if clip.begin <= time and clip.end >= time
            ),
            None,
        )

    def last_mute(self):
        mutes = self.get_mutes()
        if mutes:
            return mutes[-1]

    def from_emacs(self, video_file: str, elements):
        # [['eve-clip', [28062, 36310]], ['eve-text', ['List Delete or Install a Formula', 1]], ['eve-clip', [41344, 50448]], ['eve-clip', [54518, 59552]], ['eve-clip', [71656, 79903]], ['eve-text', ['List Installed Taps', 1]], ['eve-clip', [83116, 87294]], ['eve-text', ['Operations in Tap', 1]], ['eve-clip', [99933, 109251]], ['eve-clip', [88043, 98112]], ['eve-clip', [112571, 119319]]]
        self.edit_elements = []
        for element in elements:
            type = EditElementType(element[0])
            match type:
                case EditElementType.CLIP:
                    self.add_clip(video_file, element[1][0], element[1][1])
                case EditElementType.TEXT:
                    self.add_text(element[1][0], element[1][1])
                case EditElementType.IMAGE:
                    self.add_image(element[1][0], element[1][1])
                case EditElementType.MUTE:
                    self.add_mute(
                        self.search_clip(element[1][0]), element[1][0], element[1][1]
                    )
                    print(self.edit_elements[0].mutes)
                case _:
                    print("Not Support")
