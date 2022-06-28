from sys import argv
from hashlib import sha256

text: bytes = (argv[1] if len(argv) else "Lorem ipsum dolor sit amet").encode()

print(sha256(text).hexdigest())
