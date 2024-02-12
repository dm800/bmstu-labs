import random


def generate_combination(num):
    sp = []
    values = list("`~1!2@3#4$5%6^7&8*9(0)-_=+qQwWeErRtTyYuUiIoOpP[{]}"
                  "\\|aAsSdDfFgGhHjJkKlL;:'\"zZxXcCvVbBnNmM,<.>/?")
    for i in range(num):
        sp.append(random.choice(values))
    return "".join(sp)
