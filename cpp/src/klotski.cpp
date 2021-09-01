#include "klotski.hpp"
#include <vector>
#include <map>
#include <queue>
#include <iostream>

std::vector<std::pair<char, std::pair<Situation, Situation>>> trackResult(const std::map<Situation, std::pair<Situation, char>> &out, Situation &end);
bool isSuccess(const Situation &sit, std::vector<int> dest, char name);

Klotski::Klotski(int nr, int nc)
{
    this->nr = nr;
    this->nc = nc;
    this->size = nr * nc;
    this->names = std::map<std::string, char>();
    this->names2 = std::map<char, std::string>();
    this->idx = 0;
    this->start = Situation(new char[nr * nc], nr *nc);
    this->count = 1;
    this->result = std::vector<Move>();
    this->names2[0] = '.';
}

// void Klotski::setDestX(int x)
// {
//     this->destX = x;
// }

// void Klotski::setDestY(int y)
// {
//     this->destY = y;
// }

void Klotski::setTarget(std::string name)
{
    this->target = this->names.find(name)->second;
}

void Klotski::setDestPost(int x, int y)
{
    int top = 100;
    int left = 100;
    std::vector<int> idxs = std::vector<int>();
    for (int i = 0; i < nr * nc; i++)
    {
        if (this->start.m_sit[i] != this->target)
        {
            continue;
        }
        idxs.push_back(i);
        int ix = i / nc;
        int iy = i % nc;
        if (ix < top)
            top = ix;
        if (iy < left)
            left = iy;
    }
    int topDiff = x - top;
    int leftDiff = y - left;
    for (int i = 0; i < idxs.size(); i++)
    {
        int oldX = idxs.at(i) / nc;
        int oldY = idxs.at(i) % nc;
        int newX = oldX + topDiff;
        int newY = oldY + leftDiff;
        this->destPos.push_back(newX * nc + newY);
    }
}

void Klotski::updateSituation(std::string name)
{
    if (idx == nr * nc)
    {
        return;
    }
    if (name.at(0) == '.')
    {
        this->start.m_sit[idx] = 0;
    }
    else
    {
        if (this->names.count(name) == 0)
        {
            // this->names.insert(std:pair<std::string,int>(name, this->count));
            this->names[name] = this->count;
            this->names2[this->count] = name;
            this->count = this->count + 1;
        }
        this->start.m_sit[idx] = this->names.find(name)->second;
    }
    this->idx = this->idx + 1;
}

void Klotski::klotski()
{
    std::queue<Situation> queue = std::queue<Situation>();
    std::set<Situation> seen = std::set<Situation>();
    std::map<Situation, std::pair<Situation, char>> out;
    queue.push(this->start);
    seen.insert(this->start);
    std::optional<Situation> res = bfs1(queue, seen, out);
    if (res.has_value()) {
        // printSituation(res.value());
        std::vector<std::pair<char, std::pair<Situation, Situation>>> steps = trackResult(out, res.value());
        // for (int i = steps.size() - 1; i >= 0; i--) {
        //     printSituation(steps.at(i).second.first);
        // }
    }
}

std::vector<std::pair<char, std::pair<Situation, Situation>>> trackResult(const std::map<Situation, std::pair<Situation, char>> &out, Situation &end) {
    std::vector<std::pair<char, std::pair<Situation, Situation>>> steps = std::vector<std::pair<char, std::pair<Situation, Situation>>>();
    Situation k = end;
    std::cout << "map size: " << out.size() << std::endl;
    while(out.count(k) != 0) {
        std::pair<Situation, char> v = out.at(k);
        steps.push_back(std::make_pair(v.second, std::make_pair(v.first, k)));
        k = v.first;
    }
    std::cout << "result length: " << steps.size() << std::endl;
    return steps;
}

std::optional<Situation> Klotski::bfs1(std::queue<Situation> &queue, std::set<Situation> &seen, std::map<Situation, std::pair<Situation, char>> &out)
{
    if (queue.empty())
    {
        return std::nullopt;
    }
    Situation curSit = queue.front();
    queue.pop();
    std::vector<std::pair<Situation, char>> validMoves = findValidMoves(curSit, seen);
    for (auto sit : validMoves) {
        out[sit.first] = std::make_pair(curSit, sit.second);
        queue.push(sit.first);
        if (sit.second == this->target && isSuccess(sit.first)) {
            return std::make_optional(sit.first);
        }
    }
    return bfs1(queue, seen, out);
}

bool Klotski::isSuccess(const Situation &sit)
{
    for (int i : this->destPos)
    {
        if (sit.m_sit[i] != this->target)
        {
            return false;
        }
    }
    printSituation(sit);
    return true;
}

std::vector<std::pair<Situation, char>> Klotski::findValidMoves(const Situation &sit, std::set<Situation> &seen)
{
    std::vector<std::pair<Situation, char>> result = std::vector<std::pair<Situation, char>>();
    for (auto const &[key, val] : this->names)
    {
        searchValidMovement(seen, sit, val, result);
    }
    return result;
}

void Klotski::searchValidMovement(std::set<Situation> &seen, const Situation &sit,
                                  char name, std::vector<std::pair<Situation, char>> &result)
{
    std::queue<Situation> q = std::queue<Situation>();
    q.push(sit);
    bfs2(seen, q, result, name);
}

void Klotski::bfs2(std::set<Situation> &seen, std::queue<Situation> &q, std::vector<std::pair<Situation, char>> &result, char name)
{
    if (q.empty()) {
        return;
    }
    std::vector<std::pair<int, int>> idxs_l = std::vector<std::pair<int, int>>();
    std::vector<std::pair<int, int>> idxs_r = std::vector<std::pair<int, int>>();
    std::vector<std::pair<int, int>> idxs_u = std::vector<std::pair<int, int>>();
    std::vector<std::pair<int, int>> idxs_d = std::vector<std::pair<int, int>>();
    Situation &sit = q.front();
    q.pop();
    for (int i = 0; i < sit.m_size; i++)
    {
        if (sit.m_sit[i] == name)
        {
            idxs_l.push_back(moveLeft(i, this->nc));
            idxs_r.push_back(moveRight(i, this->nc));
            idxs_u.push_back(moveUp(i, this->nc));
            idxs_d.push_back(moveDown(i, this->nc));
        }
    }

    auto f = [this, sit, &seen, &q, name, &result](std::vector<std::pair<int, int>> idxs)
    {
        if (isInBound(idxs, this->nr, this->nc) && isNoBarrier(idxs, name, sit, this->nc))
        {
            Situation newSit = sit.clone();
            for (int i = 0; i < newSit.m_size; i++)
            {
                if (newSit.m_sit[i] == name)
                {
                    newSit.m_sit[i] = 0;
                }
            }
            for (auto pair : idxs)
            {
                int idx = pair.first * this->nc + pair.second;
                newSit.m_sit[idx] = name;
            }
            if (seen.count(newSit) == 0)
            {
                q.push(newSit);
                seen.insert(newSit);
                result.push_back(std::make_pair(newSit, name));
            }
        }
    };
    f(idxs_l);
    f(idxs_r);
    f(idxs_u);
    f(idxs_d);
    bfs2(seen, q, result, name);
}

void Klotski::printSituation(const Situation &sit) {

    for(int i = 0; i < sit.m_size; i ++) {
        std::cout<<this->names2.find(sit.m_sit[i])->second;
        if ((i+1) % this->nc == 0) {
            std::cout << std::endl;
        }
    }
    std::cout<<std::endl;
}