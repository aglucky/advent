import argparse
def tree(size, stump, level=0) :
    match size:
        case size if size > level:
            spaces = " " * (size - level)
            stars =  "*" * (1 + 2 * level)
            print(f"{spaces}{stars}{spaces}")
            tree(size, stump, level+1)
        case _ :
            for i in range(stump):
                spaces = " " * (size)
                print(f"{spaces}*{spaces}")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--size", help="The size of the tree", type=int, default=10)
    parser.add_argument("--stump", help="The height of the tree stump", type=int, default=3)
    args = parser.parse_args()
    tree(args.size, args.stump)

if __name__ == "__main__":
    main()