#!/usr/bin/env python3
"""A tiny GTP engine for etaf-go tests: plays the first legal point."""
import sys

SIZE = 19
LETTERS = "ABCDEFGHJKLMNOPQRSTUVWXYZ"


def parse_vertex(text):
    text = text.strip().lower()
    if not text or text == "pass":
        return None
    column = LETTERS.find(text[0].upper())
    row = int(text[1:])
    return (row - 1, column)


def to_vertex(point):
    row, column = point
    return f"{LETTERS[column]}{row + 1}"


def main():
    board = {}
    size = SIZE
    while True:
        line = sys.stdin.readline()
        if not line:
            return 0
        parts = line.strip().split()
        if not parts:
            continue
        command, arguments = parts[0].lower(), parts[1:]
        if command == "name":
            print("= test-engine\n")
        elif command == "version":
            print("= 0.1\n")
        elif command == "boardsize":
            size = int(arguments[0])
            board.clear()
            print("=\n")
        elif command in ("komi", "time_settings", "level"):
            print("=\n")
        elif command in ("clear_board", "clear"):
            board.clear()
            print("=\n")
        elif command == "play":
            color, vertex = arguments[0].lower(), arguments[1]
            point = parse_vertex(vertex)
            if point:
                board[point] = color
            print("=\n")
        elif command == "genmove":
            color = arguments[0].lower()
            choice = None
            for row in range(size):
                for column in range(size):
                    point = (row, column)
                    if point not in board:
                        choice = point
                        break
                if choice:
                    break
            if choice is None:
                print("= pass\n")
            else:
                board[choice] = color
                print(f"= {to_vertex(choice)}\n")
        elif command == "quit":
            print("=\n")
            return 0
        else:
            print("? unknown command\n")
        sys.stdout.flush()


if __name__ == "__main__":
    sys.exit(main())
