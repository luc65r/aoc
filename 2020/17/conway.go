package main

import (
    "bufio"
    "fmt"
    "os"
)

type loc struct {
    x, y, z int
}

type cube bool
type state map[loc]cube

const (
    ACTIVE = true
    INACTIVE = false
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)

    state := make(state)
    l := loc{0, 0, 0}
    for scanner.Scan() {
        for _, c := range scanner.Text() {
            switch c {
            case '.':
            case '#':
                state[l] = ACTIVE
            default:
                break
            }
            l.x++
        }
        l.x = 0
        l.y++
    }

    if err := scanner.Err(); err != nil {
        fmt.Fprintln(os.Stderr, err)
        os.Exit(1)
    }

    for i := 0; i < 6; i++ {
        state.next()
    }

    fmt.Println(len(state))
}

func (s state) count(l loc) (n int) {
    for _, p := range l.neighbors() {
        if s[p] {
            n++
        }
    }

    return
}

func (s state) next() {
    diff := make(state)
    for p, c := range s {
        diff[p] = c.next(s.count(p))
        for _, q := range p.neighbors() {
            diff[q] = s[q].next(s.count(q))
        }
    }

    for p, c := range diff {
        if c {
            s[p] = ACTIVE
        } else {
            delete(s, p)
        }
    }
}

func (c cube) next(n int) cube {
    if c {
        return n == 2 || n == 3
    } else {
        return n == 3
    }
}

func (c cube) String() string {
    if c {
        return "#"
    } else {
        return "."
    }
}

func (l loc) String() string {
    return fmt.Sprint(l.x, l.y, l.z)
}

func (l loc) neighbors() [26]loc {
    return [26]loc{
        {l.x-1, l.y-1, l.z-1}, {l.x, l.y-1, l.z-1}, {l.x+1, l.y-1, l.z-1},
        {l.x-1, l.y  , l.z-1}, {l.x, l.y  , l.z-1}, {l.x+1, l.y  , l.z-1},
        {l.x-1, l.y+1, l.z-1}, {l.x, l.y+1, l.z-1}, {l.x+1, l.y+1, l.z-1},

        {l.x-1, l.y-1, l.z  }, {l.x, l.y-1, l.z  }, {l.x+1, l.y-1, l.z  },
        {l.x-1, l.y  , l.z  },                      {l.x+1, l.y  , l.z  },
        {l.x-1, l.y+1, l.z  }, {l.x, l.y+1, l.z  }, {l.x+1, l.y+1, l.z  },

        {l.x-1, l.y-1, l.z+1}, {l.x, l.y-1, l.z+1}, {l.x+1, l.y-1, l.z+1},
        {l.x-1, l.y  , l.z+1}, {l.x, l.y  , l.z+1}, {l.x+1, l.y  , l.z+1},
        {l.x-1, l.y+1, l.z+1}, {l.x, l.y+1, l.z+1}, {l.x+1, l.y+1, l.z+1},
    }
}
