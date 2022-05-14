import pandas as pd
import os
import os.path
import sys

def format_puzzle(s):
    return "\n".join(s[i:i+9].replace("0", ".") for i in range(0, 81, 9))

def write_puzzle(s, filename):
    with open(filename, "w") as f:
        f.write(format_puzzle(s))

if __name__ == "__main__":
    csv_file = sys.argv[1]
    df = pd.read_csv(csv_file)
    dirname = "valid_puzzles_and_solutions"
    dirname = os.path.join(os.path.dirname(os.path.abspath(__file__)), dirname)
    if not os.path.exists(dirname):
        os.mkdir(dirname)

    for idx, row in df.iterrows():
        write_puzzle(row.puzzle, os.path.join(dirname, f"{idx+1}.puzzle"))
        write_puzzle(row.solution, os.path.join(dirname, f"{idx+1}.solution"))
