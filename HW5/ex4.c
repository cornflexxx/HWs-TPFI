#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct {
  int16_t succ;
  int16_t prec;
} Pair;

#define BLOCK_TIME(block)                                                      \
  {                                                                            \
    clock_t begin = clock();                                                   \
    block clock_t end = clock();                                               \
    double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;                \
    printf("time spent: %f", time_spent);                                      \
  }

void initNatsPair(int n, Pair *nats) {
  while (n--)
    nats->succ = 1, nats++->prec = 1;
}

void initNatsBool(int n, bool *nats) {
  while (n--)
    *nats++ = true;
}

// crivello di eulero con vettore di Pair
Pair *eulerSieve(int n) {
  Pair *primes = malloc((n + 1) * sizeof(Pair));
  int *toDel = malloc(((n / 2) + 1) * sizeof(int));
  initNatsPair(n + 1, primes);
  int maxPrime = 2, i, j;
  while (maxPrime * maxPrime <= n) {
    i = maxPrime;
    j = 0;
    while (i * maxPrime <= n) {
      toDel[j++] = i * maxPrime;
      i += primes[i].succ;
    }
    while (j--) {
      Pair toDelPair = primes[toDel[j]];
      primes[toDel[j] - toDelPair.prec].succ += toDelPair.succ;
      if (toDel[j] + toDelPair.succ <= n)
        primes[toDel[j] + toDelPair.succ].prec += toDelPair.prec;
    }
    maxPrime += primes[maxPrime].succ;
  }
  free(toDel);
  return primes;
}

// crivello di eratostene implementato con vettore di bool
bool *erathostenesSieve(int n) {
  bool *primes = calloc(n + 1, sizeof(bool));
  initNatsBool(n + 1, primes);
  for (int p = 2; p * p <= n; p++) {
    if (*(primes + p) == true) {
      for (int i = p * p; i <= n; i += p)
        *(primes + i) = false;
    }
  }
  return primes;
}

int *linearSieve(int n, int *q) {
  int *lp = (int *)malloc((n + 1) * sizeof(int));
  int *primes = (int *)malloc(n * sizeof(int));
  int head = 0;
  for (int i = 2; i <= n; ++i) {
    if (lp[i] == 0) {
      lp[i] = i;
      primes[head++] = i;
    }
    for (int j = 0; i * primes[j] <= n; ++j) {
      lp[i * primes[j]] = primes[j];
      if (primes[j] == lp[i]) {
        break;
      }
    }
    *q = head;
  }
  return primes;
}

void printPrimesPair(int n) {
  Pair *sieve = eulerSieve(n);
  int i = 2;
  while (i <= n) {
    printf("%d ", i);
    i += (sieve + i)->succ;
  }
  printf("\n");
}

void printPrimesBool(int n) {
  bool *sieve = erathostenesSieve(n);
  for (int i = 2; i <= n; i++) {
    if (*(sieve + i))
      printf("%d ", i);
  }
  printf("\n");
}

void printPrimesLinear(int n) {
  int q;
  int *sieve = linearSieve(n, &q);
  for (int i = 0; i < q; i++)
    printf("%d ", sieve[i]);
  printf("\n");
}

int main(int argc, char **argv) {
  int n = atoi(argv[1]);

  clock_t begin = clock();
  printPrimesPair(n);
  clock_t end = clock();
  double time_spent_euler = (double)(end - begin) / CLOCKS_PER_SEC;

  begin = clock();
  printPrimesBool(n);
  end = clock();
  double time_spent_erath = (double)(end - begin) / CLOCKS_PER_SEC;

  begin = clock();
  printPrimesLinear(n);
  end = clock();
  double time_spent_linear = (double)(end - begin) / CLOCKS_PER_SEC;

  printf("Euler sieve : %f s\n", time_spent_euler);
  printf("Erathostenes sieve : %f s\n", time_spent_erath);
  printf("Linear sieve : %f s\n", time_spent_linear);
}
