## Imitating Sedgewick's Statically Typed Java Code in Python

To imitate in Python the more formal object-oriented programs in Sedgewick's book
some special libraries must be used. In Java the Comparable interface provides an ordering for sorted objects. In Python, the "rich comparison" or "total ordering" protocol provides a similar function. Python objects define special methods for comparison with other objects that can be used in sorting: 

- __lt__(self, other): Less than
- __le__(self, other): Less than or equal to
- __gt__(self, other): Greater than
- __ge__(self, other): Greater than or equal to
- __eq__(self, other): Equal to
- __ne__(self, other): Not equal to

Python type hints can also be used to imitate Java static type checking, resulting in the following code: 

```Python
from typing import TypeVar, List, Callable, Optional

T = TypeVar('T')

def sort(a: List[T], comparator: Optional[Callable[[T, T], int]] = None) -> None:
    """
    Rearranges the list in ascending order, using the natural order or a comparator.

    Args:
        a (List[T]): The list to be sorted.
        comparator (Optional[Callable[[T, T], int]]): The comparator specifying the order. Defaults to None,
            which means natural order will be used.
    """
    n = len(a)
    for i in range(n):
        min_idx = i
        for j in range(i+1, n):
            if comparator:
                if comparator(a[j], a[min_idx]) < 0:
                    min_idx = j
            else:
                if a[j] < a[min_idx]:
                    min_idx = j
        a[i], a[min_idx] = a[min_idx], a[i]
        assert is_sorted(a, comparator, 0, i)

    assert is_sorted(a, comparator)

def is_sorted(a: List[T], comparator: Optional[Callable[[T, T], int]] = None,
              lo: int = 0, hi: int = None) -> bool:
    """
    Checks if the list is sorted in ascending order.

    Args:
        a (List[T]): The list to be checked.
        comparator (Optional[Callable[[T, T], int]]): The comparator specifying the order. Defaults to None,
            which means natural order will be used.
        lo (int): The starting index of the list. Defaults to 0.
        hi (int): The ending index of the list. Defaults to None, which means the last index of the list.

    Returns:
        bool: True if the list is sorted, False otherwise.
    """
    hi = hi if hi is not None else len(a) - 1
    for i in range(lo + 1, hi + 1):
        if comparator:
            if comparator(a[i], a[i-1]) < 0:
                return False
        else:
            if a[i] < a[i-1]:
                return False
    return True

def main() -> None:
    """
    Reads in a sequence of strings from standard input, sorts them using selection sort,
    and prints them to standard output in ascending order.
    """
    a = input().split()  # assuming input is space-separated strings
    sort(a)
    print(a)

if __name__ == "__main__":
    main()
```

**Output:**

![typed](https://user-images.githubusercontent.com/68504324/232930732-35c3f5a1-cfd2-4368-b578-b07475782ebd.jpg)
