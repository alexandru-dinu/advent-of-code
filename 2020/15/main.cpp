#include <iostream>
#include <vector>
#include <unordered_map>
#include <fstream>
#include <string>
#include <algorithm>


int go(const std::vector<int>& xs, size_t limit)
{
    int prev = xs.back();

    std::unordered_map<int, int> history;
    for (size_t i = 0; i < xs.size(); ++i) {
        history[xs[i]] = i + 1;
    }

    for (size_t i = xs.size(); i < limit; ++i) {
        int c = prev;
        auto it = history.find(c);
        prev = (it == history.end()) ? 0 : (i - it->second);
        history[c] = i;
    }

    return prev;
}

std::vector<int> get_input(const std::string &file) {
    // the local vector will be moved on return
    std::vector<int> xs;

    std::ifstream ifs(file);
    std::string line;
    while (std::getline(ifs, line, ',')) {
        line.erase(
            std::remove_if(line.begin(), line.end(), ::isspace),
            line.end());
        xs.push_back(std::stoi(line));
    }

    ifs.close();

    // explicit std::move(xs) will prevent copy elision
    return xs;
}

int main(int argc, char const *argv[])
{
    if (argc != 2) {
        std::cerr << "Usage: ./main <input>\n";
        return -1;
    }

    std::vector<int> xs = get_input(argv[1]);

    std::cout << "Part 1: " << go(xs, 2020) << "\n";
    std::cout << "Part 2: " << go(xs, 30000000) << "\n";

    return 0;
}
