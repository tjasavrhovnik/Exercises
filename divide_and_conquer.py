##########################################################################
# We wish to define pivoting (in-place) for an array a. Because we want
# to pivot only parts of the array, we restrict our function to only
# modify the array between the indices start and end.
# For instance, if start = 0 and end = 8 the array
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# is pivoted to
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (There are many solutions, the important part is that 10 is a pivot.)
#
# Define a function pivot_list(a, start, end) that pivots the array a
# between indices start and end in such a way that a[start] becomes
# the pivot of that part.
# The function should return the index of the pivot AFTER the pivoting.
# It should work in time O(n) where n is the lenght of a and in-place.
# Example:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot_list(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##########################################################################
def pivot_list(a, start, end):
    # save pivot
    pivot = a[start]
    # save pointers
    front_i = start
    back_i = end
    # move pointers and change elements if needed
    while front_i != back_i:
        if a[front_i + 1] <= pivot:
            front_i += 1
        elif a[back_i] > pivot:
            back_i -= 1
        else:
            temp = a[front_i + 1]
            a[front_i + 1] = a[back_i]
            a[back_i] = temp
    # move pivot
    a[start] = a[front_i]
    a[front_i] = pivot
    return front_i

##########################################################################
# We wish to implement quicksort.
#
# Define a function quicksort(a) that sorts the array a using pivoting.
# Make sure that it works in-place.
#
# Hint: Define a function quicksort_part(a, start, end) that sorts only
#       a part of the array.
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##########################################################################
def quicksort_part(a, start, end):
    if start >= end:
        return
    b = pivot_list(a, start, end)
    quicksort_part(a, start, b-1)
    quicksort_part(a, b+1, end)

def quicksort(a):
    quicksort_part(a, 0, len(a)-1)
    return a

##########################################################################
# We are searching for the k-th smallest element of the array a.
# For instance, if
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# then the third element is 5, because 2, 3 and 4 are smaller.
# Indexing starts at 0, so the 0-th element is 2.
#
# Define the function kth_element(a, k) that finds the k-th smallest
# element of a. The array is allowed to change.
#
# Hint: You can use an auxilary function.
##########################################################################
def kth_part(a, k, start, end):
    if start >= end:
        return
    b = pivot_list(a, start, end)
    

def kth_element(a, k):
    kth_part(a, k, 0, len(a)-1)
    return k
