#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//merge ricorsiva
void merge(int *dx, int *sx, int ld, int ls, int *mx) {
  if (ld && ls) {
    if (*dx <= *sx) {
      *mx = *dx;
      merge(dx + 1, sx, ld - 1, ls, mx + 1);
    } else {
      *mx = *sx;
      merge(dx, sx + 1, ld, ls - 1, mx + 1);
    }
  } else {
    while (ld--)
      *mx++ = *dx++;
    while (ls--)
      *mx++ = *sx++;
  }
}

//merge iterativa
void mergeI(int *dx, int *sx, int ld, int ls, int *mx) {
  while (ld && ls) {
    if (*dx <= *sx)
      *mx++ = *dx++, ld--;
    else
      *mx++ = *sx++, ls--;
  }
  while (ld--)
    *mx++ = *dx++;
  while (ls--)
    *mx++ = *sx++;
}

//trova le sequenze ordinate e le memorizza in un array kx, ritorna il
//numero di sequenze k
int *ordSeq(int *ax, int la, int *k) {
  int *kx = (int *)calloc(la, sizeof(int));
  *k = 0;
  kx[0] = 1;
  for (int j = 1; j < la; j++) {
    if (ax[j] >= ax[j - 1])
      kx[*k]++;
    else {
      *k += 1;
      kx[*k] = 1;
    }
  }
  *k += 1;
  return kx;
}

//2.2 merge sort con O(nlogk) divide l'array in sequenze ordinate massime
int mergeSortEff(int *ax, int la) {
  int *kx, k;
  int *mx = (int *)malloc(sizeof(int) * la);
  int *ori_ax = ax;
  kx = ordSeq(ax, la, &k);
  while (k > 1) {
    int i = 0, q = 0;
    for (int j = 0; j + 1 < k; j += 2) {
      merge(ax + i, ax + i + kx[j], kx[j], kx[j + 1], mx + i);
      kx[q++] = kx[j] + kx[j + 1];
      i += kx[q - 1];
    }
    if (k % 2 == 1) {
      for (int z = 0; z < kx[k - 1]; z++)
        mx[i + z] = ax[i + z];
      kx[q++] = kx[k - 1];
      i += kx[q - 1];
    }
    k = q;
    int *tmp = ax;
    ax = mx, mx = tmp;
  }
  memcpy(ori_ax, ax, sizeof(int) * la);
  free(kx);
  free(mx);
  return 0;
}

// 2.1) merge sort iterativo , con merge ricorsivo o merge iterativo (mergeI)
int mergeSort(int *ax, int la) {
  int *mx = (int *)malloc(sizeof(int) * la);
  for (int offs = 1; offs < la; offs <<= 1) {
    for (int i = 0; i < la; i += 2 * offs) {
      int m = (i + offs > la) ? la : i + offs;
      int e = (m + offs > la) ? la : m + offs;
      merge(ax + i, ax + m, m - i, e - m, mx + i);
    }
    int *tmp = ax;
    ax = mx, mx = tmp;
  }
  free(mx);
  return 0;
}

int main(void) {
  int ax[] = {1, 2, 33, 7, 1, 2, 59, 1, 24};
  int n = sizeof(ax) / sizeof(ax[0]);
  mergeSortEff(ax, n);
  for (int i = 0; i < n; i++) {
    printf("%d ", ax[i]);
  }
  printf("\n");
  return 0;
}
