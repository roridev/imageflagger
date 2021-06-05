from dataclasses import dataclass
from shared.core.encoder import getHashFromStr


@dataclass(frozen=True)
class ImageData:
    data: str

    def getHash(self):
        return getHashFromStr(self.data)

    def __str__(self):
        return self.data

    @staticmethod
    def deserialize(xs: list[str]):
        return ImageData(xs[0])
