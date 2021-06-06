import os
from dataclasses import dataclass, field

from shared.core.encoder import comma, uncomma
from shared.core.imagedata import ImageData
from shared.core.imageentry import ImageEntry
from shared.core.tag import Tag


@dataclass
class Repository:
    root: str
    head: str = ""
    objects: list[str] = field(default_factory=list)
    tags: dict[str, Tag] = field(default_factory=dict)
    entries: dict[str, ImageEntry] = field(default_factory=dict)
    index: dict[str, list[str]] = field(default_factory=dict)

    def addEntry(self, entry: ImageEntry):
        if entry.getHash() in self.entries:
            print("Nothing to do.")
            return

        if entry.image not in self.objects:
            print(f"Reference to invalid image detected. {entry.image} does not exist in the repo.")
            return

        self.entries[entry.getHash()] = entry
        print(f"Added [{entry.getHash()}] {entry.name} {[self.unrefTag(r) for r in entry.tags]}\n"
              f"{entry.description}\n"
              f"Source : {entry.source}")

    def addTag(self, tag: Tag):
        if tag.getHash() in self.tags:
            print("Nothing to do.")
            return

        self.tags[tag.getHash()] = tag
        print(f"Tag {tag.name} ({tag.getHash()}) added!")

    def addObject(self, data: ImageData):
        if data.getHash() in self.objects:
            print("Nothing to do.")
            return

        self.objects.append(data.getHash())
        print(f"Object [{data.getHash()}] added!")

    def __str__(self):
        return f"{comma(self.objects)}\n" \
               f"{comma(self.tags.keys())}\n" \
               f"{comma(self.entries.keys())}\n"

    def unrefTag(self, r: str) -> str:
        return self.tags[r].name

    # THE DEAD LANDS OF SPOOKY IO #

    def IO_populateIndex(self):
        with open(f"{self.root}/index", "w+") as fh:
            fh.writelines(str(self))

    def IO_readIndex(self):
        with open(f"{self.root}/index", "r+") as fh:
            lines: list[str] = fh.readlines()
            self.index["objects"] = uncomma(lines[0])
            self.index["tags"] = uncomma(lines[1])
            self.index["entries"] = uncomma(lines[2])

        for item in self.index["objects"]:
            self.objects.append(item)

        for item in self.index["entries"]:
            self.entries[item] = self.IO_tryLoadInfo("entries", item)

        for item in self.index["tags"]:
            self.tags[item] = self.IO_tryLoadInfo("tags", item)

        return None

    def IO_populateHead(self):
        with open(f"{self.root}/HEAD", "w+") as fh:
            fh.writelines(self.head)

    def IO_readHead(self):
        with open(f"{self.root}/HEAD", "r+") as fh:
            self.head = fh.readlines()[0]

    def IO_tryLoadInfo(self, t: str, h: str):
        with open(f"{self.root}/{t}/{h[:2]}/{h}", "r+") as fh:
            lines: list[str] = fh.readlines()
            if t == "entries":
                return ImageEntry.deserialize(lines)
            elif t == "tags":
                return Tag.deserialize(lines)
            elif t == "objects":
                return ImageData.deserialize(lines)
            else:
                raise ValueError(f"Unsupported type: {t}")

    def IO_trySaveInfo(self, t: str, d):
        h: str = d.getHash()
        os.makedirs(f"{self.root}/{t}/{h[:2]}", exist_ok=True)
        with open(f"{self.root}/{t}/{h[:2]}/{h}", "w+") as fh:
            fh.writelines(str(d))

    def IO_flush(self):
        for item in [i for i in self.entries.keys() if i not in self.index["entries"]]:
            print(f"Flushing new entry [{item}]")
            self.IO_trySaveInfo("entries", self.entries[item])

        for item in [i for i in self.tags.keys() if i not in self.index["tags"]]:
            print(f"Flushing new tag [{item}]")
            self.IO_trySaveInfo("tags", self.tags[item])

    def IO_init(self):
        if os.path.exists(f"{self.root}/index"):
            raise AssertionError("An repository was already created on this folder.")

        if not os.path.exists(self.root):
            os.makedirs(self.root)

        self.IO_populateIndex()
        self.IO_populateHead()
        print(f"An new repository was created in {self.root}.")
