from dataclasses import dataclass

from shared.core.encoder import getHashFromStr


@dataclass
class Tag:
    name: str

    def getHash(self):
        return getHashFromStr(self.name)

    def __str__(self):
        return self.name

    @staticmethod
    def deserialize(xs: list[str]):
        return Tag(xs[0])
