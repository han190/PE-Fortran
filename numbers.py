import sys

def integerArray(val):
    longString = ''
    for i in range(1, val):
        longString += str(i).zfill(4) + ' '
    longString += str(val).zfill(4)
    return longString

def main():
    print(integerArray(int(str(sys.argv[1]))))

if __name__ == "__main__":
    main()