import sys
import os, os.path


def count_files(dir):
    def satisfied(x):
        return os.path.isfile(os.path.join(dir, x))

    return len([name for name in os.listdir(dir) if satisfied(name)])


def integer_array(val):
    longString = ""
    for i in range(1, val):
        longString += str(i).zfill(4) + " "
    longString += str(val).zfill(4)
    return longString


def main():
    current_directory = "./src/euler/"
    nfiles = count_files(current_directory)
    print(integer_array(nfiles))


if __name__ == "__main__":
    main()
