from enum import Enum, unique

@unique
class EditElementType(Enum):
    CLIP = "eaf-ve-clip"
    TEXT = "eaf-ve-text"
    IMAGE = "eaf-ve-image"

class EditElement:
    def __init__(self, type):
        self.type = type

class EditClip(EditElement):
    def __init__(self, file, begin, end):
        EditElement.__init__(self, EditElementType.CLIP)
        self.file = file
        self.begin = begin
        self.end = end
    def range_str(self):
        return f"[{self.begin} -> {self.end}]"

    def to_simple(self):
        return [self.begin, self.end]


class EditText(EditElement):
    def __init__(self, text, duration):
        EditElement.__init__(self, EditElementType.TEXT)
        self.text = text
        self.duration = duration

class EditElements :
        def __init__(self):
            self.edit_elements = []

        def get_by_type(self, type):
            return [x for x in self.edit_elements if x.type == type]

        def get_clips(self):
            return self.get_by_type(EditElementType.CLIP)

        def get_texts(self):
            return self.get_by_type(EditElementType.TEXT)

        def add_clip(self, file, begin, end):
            self.edit_elements.append(EditClip(file, begin, end))

        def add_text(self, text, duration):
            self.edit_elements.append(EditText(text, duration))

        def last_clip(self):
            clips = self.get_clips()
            if clips:
                return clips[-1]

        def first_clip(self):
            clips = self.get_clips()
            if clips:
                return clips[0]
        def from_emacs(self, video_file, elments):
            # [['eaf-ve-clip', [28062, 36310]], ['eaf-ve-text', ['List Delete or Install a Formula', 1]], ['eaf-ve-clip', [41344, 50448]], ['eaf-ve-clip', [54518, 59552]], ['eaf-ve-clip', [71656, 79903]], ['eaf-ve-text', ['List Installed Taps', 1]], ['eaf-ve-clip', [83116, 87294]], ['eaf-ve-text', ['Operations in Tap', 1]], ['eaf-ve-clip', [99933, 109251]], ['eaf-ve-clip', [88043, 98112]], ['eaf-ve-clip', [112571, 119319]]]
            self.edit_elements = []
            for element in elments:
                type = EditElementType(element[0])
                match type:
                    case EditElementType.CLIP:
                        self.add_clip(video_file, element[1][0], element[1][1])
                    case EditElementType.TEXT:
                        self.add_text(element[1][0], element[1][1])
                    case EditElementType.IMAGE:
                        print("Not Support")
                    case _:
                        print("Not Support")
