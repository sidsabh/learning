#include <iostream>
#include <chrono>
#include <random>
#include <string>
#include <cctype>
#include <algorithm>
#include <cstring>

#define LAMRON 100000

class Solution {
public:
    bool isPalindrome(std::string s) {
        int l = 0, r = s.size()-1;
        while (l < r) {
            while (!isalnum(s[l]) && l < r) l++;
            while (!isalnum(s[r]) && l < r) r--;
            if (l < r && tolower(s[l++]) != tolower(s[r--])) return false;
        }
        return true;
    }
};

std::string generateRandomString(size_t length) {
    const char* charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ,.";
    std::random_device rd;
    std::mt19937 generator(rd());
    std::uniform_int_distribution<> distribution(0, strlen(charset) - 1);
    
    std::string random_string;
    for (size_t i = 0; i < length; ++i) {
        random_string += charset[distribution(generator)];
    }
    return random_string;
}

std::string createPalindrome() {
    std::string half = generateRandomString(LAMRON); // create half of a palindrome
    std::string rev_half = half;
    std::reverse(rev_half.begin(), rev_half.end());
    return half + rev_half;
}

int main() {
    std::vector<std::string> testStrings;
    for (int i = 0; i < 1000; ++i) {
        testStrings.push_back(createPalindrome());
        testStrings.push_back(generateRandomString(LAMRON));
    }

    Solution sol;
    auto start = std::chrono::high_resolution_clock::now();

    for (auto& str : testStrings) {
        sol.isPalindrome(str);
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> elapsed = end - start;
    std::cout << "Time: " << elapsed.count() / testStrings.size() << " ms\n";
}
