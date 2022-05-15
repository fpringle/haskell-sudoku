import os
import os.path
import sys


if __name__ == "__main__":
    try:
        file = sys.argv[1]
        chunks = int(sys.argv[2])
        chunksize = int(sys.argv[3])
    except:
        print(f"USAGE: {__file__} infile chunks chunk-size [dirname]", file=sys.stderr, flush=1)
        exit(1)

    if len(sys.argv) > 4:
        dirname = sys.argv[4]
        if not os.path.exists(dirname):
            os.mkdir(dirname)
    else:
        dirname = os.path.dirname(os.path.abspath(__file__))

    with open(file) as f:
        header = f.readline()
        eof = False
        for chunk in range(1, chunks+1):
            chunk_lines = []
            for _ in range(chunksize):
                line = f.readline()
                if line == "":
                    eof = True
                    break
                else:
                    chunk_lines.append(line)

            if chunk_lines:
                start = (chunk - 1) * chunksize
                end = start + len(chunk_lines) - 1
                with open(os.path.join(dirname, f"chunk_{start}_{end}.csv"), "w") as chunkfile:
                    chunkfile.write(header)
                    chunkfile.writelines(chunk_lines)

            if eof:
                break
