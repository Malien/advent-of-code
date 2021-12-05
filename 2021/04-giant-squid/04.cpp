#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <array>

std::vector<int> parseBingoSequence(std::istream& input) {
    std::vector<int> bingoSeq;
    std::string line;
    std::string buffer;
    std::getline(input, line);
    std::istringstream lineStream(line);
    while (std::getline(lineStream, buffer, ',')) {
        bingoSeq.push_back(std::atoi(buffer.c_str()));
    }
    return bingoSeq;
}

using Board = std::array<int, 25>;

Board parseBoard(std::istream& input) {
    Board board;
    std::string line;
    std::string buffer;
    for (size_t i = 0; i < 5; ++i) {
        std::getline(input, line);
        std::istringstream lineStream(line);
        size_t j = 0;
        while (std::getline(lineStream, buffer, ' ') && j != 5) {
            if (buffer.empty()) continue;
            board[i * 5 + j] = std::atoi(buffer.c_str());
            j++;
        }
    }
    return board;
}

auto parseProblem(std::istream& input) {
    std::vector<int> bingoSeq = parseBingoSequence(input);
    std::string dummy;
    std::vector<Board> boards;
    while (std::getline(input, dummy)) {
        boards.push_back(parseBoard(input));
    }
    return std::pair(bingoSeq, boards);
}

int main() {
    std::fstream file("test.txt");
    const auto&& [bingoSeq, boards] = parseProblem(file);
}

