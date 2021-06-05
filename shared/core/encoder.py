import base64
import hashlib


def IO_tob64string(path: str) -> str:
    with open(path, "rb") as fh:
        b64d = base64.b64encode(fh.read())
        return b64d.decode('utf-8')


def getHashFromStr(s: str) -> str:
    return hashlib.sha1(s.encode()).hexdigest()


def comma(ls: iter) -> str:
    return ",".join(ls)


def uncomma(s: str) -> list[str]:
    if s == "\n":
        return []
    return s.strip().split(",")
