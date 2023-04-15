import functools

def sort(a):
    n = len(a)
    for i in range(n):
        min_idx = i
        for j in range(i+1, n):
            if less(a[j], a[min_idx]):
                min_idx = j
        exch(a, i, min_idx)
        assert is_sorted(a, 0, i)
    assert is_sorted(a)

def sort_with_comparator(a, comparator):
    n = len(a)
    for i in range(n):
        min_idx = i
        for j in range(i+1, n):
            if less_with_comparator(comparator, a[j], a[min_idx]):
                min_idx = j
        exch(a, i, min_idx)
        assert is_sorted_with_comparator(a, comparator, 0, i)
    assert is_sorted_with_comparator(a, comparator)

def less(v, w):
    return v < w

def less_with_comparator(comparator, v, w):
    return comparator(v, w) < 0

def exch(a, i, j):
    a[i], a[j] = a[j], a[i]

def is_sorted(a, lo=0, hi=None):
    if hi is None:
        hi = len(a) - 1
    for i in range(lo + 1, hi + 1):
        if less(a[i], a[i-1]):
            return False
    return True

def is_sorted_with_comparator(a, comparator, lo=0, hi=None):
    if hi is None:
        hi = len(a) - 1
    for i in range(lo + 1, hi + 1):
        if less_with_comparator(comparator, a[i], a[i-1]):
            return False
    return True

def show(a):
    for item in a:
        print(item)

# Example usage
if __name__ == '__main__':
    a = ["example", "of", "sorting", "with", "comparable"]
    sort(a)
    show(a)
