from dataclasses import dataclass, field

from shared.core.encoder import getHashFromStr, comma, uncomma

@dataclass
class ImageEntry:
    name: str
    image: str
    parents: list[str] = field(default_factory=list)
    tags: list[str] = field(default_factory=list)
    source: str = None
    description: str = None

    def getHash(self):
        return getHashFromStr(str(self))

    def __str__(self):
        return f"{self.name}\n" \
               f"{self.image}\n" \
               f"{comma(self.parents)}\n" \
               f"{comma(self.tags)}\n" \
               f"{self.description}\n" \
               f"{self.source}\n" \
               f"{self.description}\n" \
               f""

    @staticmethod
    def deserialize(xs: list[str]):
        return ImageEntry(xs[0],
                          xs[1],
                          uncomma(xs[2]),
                          uncomma(xs[3]),
                          xs[4],
                          xs[5])
