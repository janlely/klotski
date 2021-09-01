#include <string>
#include <vector>
#include <set>
#include <queue>
#include <map>
#include <optional>
#include <cassert>
struct Move {
    std::string name;
    std::vector<std::string> start;
    std::vector<std::string> end;
};

struct Situation {
    char *m_sit;
    int m_size;
    Situation(char* sit, int size):m_sit(sit), m_size(size) {}
    Situation():m_sit(0), m_size(0) {}
    bool operator<(const struct Situation &right) const {
        assert(this->m_size == right.m_size);
        for (int i = 0; i < this->m_size; i++) {
            if (this->m_sit[i] != right.m_sit[i]) {
                return this->m_sit[i] < right.m_sit[i];
            }
        }
        return false;
    }
    Situation clone() const {
        char *sit = new char[m_size];
        for(int i = 0; i < m_size; i++) {
            sit[i] = m_sit[i];
        }
        return Situation(sit, m_size);
    }
    
    void destroy() {
        delete [] m_sit;
    }
};

class Klotski  {

    private:
        int nr;
        int nc; 
        int size;
        std::map<std::string, char> names;
        std::map<char, std::string> names2;
        // int destX;
        // int destY;
        char target;
        Situation start;
        int idx;
        char count;
        std::vector<Move> result;
        std::vector<int> destPos;

    public:
        Klotski(int nr, int nc);
        void klotski();
        void setTarget(std::string name);
        // void setDestX(int x);
        // void setDestY(int y);
        void setDestPost(int x, int y);
        void updateSituation(std::string name);
        bool isSuccess(const Situation &sit);
        std::optional<Situation> bfs1(std::queue<Situation> &, std::set<Situation> &, std::map<Situation, std::pair<Situation, char>> &);
        void bfs2(std::set<Situation> &seen, std::queue<Situation> &q, std::vector<std::pair<Situation, char>> &result, char name);
        std::vector<std::pair<Situation, char>> findValidMoves(const Situation &sit, std::set<Situation> &seen);
        void searchValidMovement(std::set<Situation> &seen, const Situation &sit, char name, std::vector<std::pair<Situation, char>> &result);
        void printSituation(const Situation &sit);

        inline std::pair<int, int> moveLeft(int idx, int nc){ return std::make_pair(idx / nc, idx % nc - 1); };
        inline std::pair<int, int> moveRight(int idx, int nc){ return std::make_pair(idx / nc, idx % nc + 1); };
        inline std::pair<int, int> moveUp(int idx, int nc){ return std::make_pair(idx / nc - 1, idx % nc); };
        inline std::pair<int, int> moveDown(int idx, int nc){ return std::make_pair(idx / nc + 1, idx % nc); };
        inline bool isInBound(std::vector<std::pair<int, int>> idxs, int nr, int nc) {
            for(auto pair : idxs) {
                if (pair.first < 0 || pair.first >= nr || pair.second < 0 || pair.second >= nc) {
                    return false;
                }
            }
            return true;
        }
        inline bool isNoBarrier(std::vector<std::pair<int, int>> idxs, char c, const Situation &sit, int nc) {
            for (auto pair : idxs) {
                int idx = pair.first * nc + pair.second;
                if (sit.m_sit[idx] != 0 && sit.m_sit[idx] != c) {
                    return false;
                }
            }
            return true;
        }
};
