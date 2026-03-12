#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct {
    int top_digit;
    unsigned int *digits;
} BigInt;

unsigned long long get_top_abs(BigInt a){
    return (unsigned int)a.top_digit & 0x7FFFFFFF;
}

unsigned int get_sign(BigInt a){
    return (unsigned int)a.top_digit >> 31;
}

void set_negative_sign(BigInt* a){
    a->top_digit |= (1U << 31);
}

void set_positive_sign(BigInt* a){
    a->top_digit &= 0x7FFFFFFF;
}

int init(BigInt *a, long long val){
    unsigned long long abs = (val < 0) ? (unsigned long long)-(val+1)+1 : (unsigned long long)val;
    unsigned int sign = (val < 0) ? (1 << (sizeof(int) * 8 - 1)) : 0;
    unsigned int low = (unsigned int)(abs & 0xFFFFFFFF);
    unsigned int high = (unsigned int)(abs >> 32);
    if(high == 0) {
        if((low & (1 << 31)) == 0){
            a->top_digit = (int)(low | sign);
            a->digits = (unsigned int*)malloc(sizeof(unsigned int));
            if(!a->digits) exit(1);
            a->digits[0] = 0;
        }
        else{
            a->top_digit = (int)((low >> 31) | sign);
            a->digits = (unsigned int*)malloc(2 * sizeof(unsigned int));
            if(!a->digits) exit(1);
            a->digits[0] = 1;
            a->digits[1] = low & 0x7FFFFFFF;
        }
    }
    else{
        a->top_digit = (int)((abs >> 31) | sign);
        a->digits = (unsigned int*)malloc(2 * sizeof(unsigned int));
        if(!a->digits) exit(1);
        a->digits[0] = 1;
        a->digits[1] = (unsigned int)(abs & 0x7FFFFFFF);
    }
    return 1;
}

void deinit(BigInt* a){
    if(a != NULL && a->digits != NULL){
        free(a->digits);
        a->digits = NULL;
    }
}

BigInt copy(const BigInt* a){
    BigInt result;
    result.top_digit = a->top_digit;
    if(!a->digits){
        result.digits = NULL;
        return result;
    }
    size_t s = (a->digits[0] + 1) * sizeof(unsigned int);
    result.digits = (unsigned int*)malloc(s);
    if(result.digits) memcpy(result.digits, a->digits, s);

    return result;
}

BigInt add_magn(BigInt a, BigInt b){
    BigInt result;
    unsigned int len = (a.digits[0] > b.digits[0] ? a.digits[0] : b.digits[0]);
    result.digits = (unsigned int*)calloc(len + 2, sizeof(unsigned int));
    result.digits[0] = len;
    
    unsigned long long carry = 0;
    for(unsigned int i = 1; i <= len; ++i){
        unsigned int val_a = (i <= a.digits[0]) ? a.digits[i] : 0;
        unsigned int val_b = (i <= b.digits[0]) ? b.digits[i] : 0;
        unsigned long long sum = val_a + val_b + carry;
        result.digits[i] = (unsigned int)(sum & 0xFFFFFFFFULL);
        carry = sum >> 32;
    }
    
    unsigned long long sum_top = (unsigned long long)get_top_abs(a) + get_top_abs(b) + carry;
    result.top_digit = (int)(sum_top & 0x7FFFFFFF);
    if(sum_top > 0x7FFFFFFF) {
        result.digits[0]++;
        result.digits = realloc(result.digits, (result.digits[0] + 1) * sizeof(unsigned int));
        result.digits[result.digits[0]] = (unsigned int)result.top_digit;
        result.top_digit = (int)(sum_top >> 31);
    }
    
    return result;
}

// |a| >= |b|
BigInt sub_magn(BigInt a, BigInt b) {
    BigInt result;
    result.digits = (unsigned int*)calloc(a.digits[0] + 1, sizeof(unsigned int));
    result.digits[0] = a.digits[0];
    long long borrow = 0;
    for(unsigned int i = 1; i <= a.digits[0]; ++i){
        long long val_a = a.digits[i];
        long long val_b = (i <= b.digits[0]) ? b.digits[i] : 0;
        long long diff = val_a - val_b - borrow;
        if(diff < 0){
            diff += 0x100000000ULL;
            borrow = 1;
        }
        else borrow = 0;
        result.digits[i] = (unsigned int)diff;
    }
    result.top_digit = (int)(get_top_abs(a) - get_top_abs(b) - (int)borrow);
    
    return result;
}

int compare_abs(BigInt a, BigInt b){
    if(a.digits[0] > b.digits[0]) return 1;
    if(a.digits[0] < b.digits[0]) return -1;
    unsigned long long top_a = get_top_abs(a), top_b = get_top_abs(b);
    if(top_a > top_b) return 1;
    if(top_a < top_b) return -1;
    for(int i = (int)a.digits[0]; i >= 1; --i){
        if(a.digits[i] > b.digits[i]) return 1;
        if(a.digits[i] < b.digits[i]) return -1;
    }

    return 0;
}

void normalize(BigInt *a){
    if(a == NULL || a->digits == NULL) return;
    unsigned int count = a->digits[0];
    while(count > 0 && a->digits[count] == 0) count--;
    if(count != a->digits[0]){
        a->digits[0] = count;
        unsigned int *tmp = (unsigned int*)realloc(a->digits, (count + 1) * sizeof(unsigned int));
        if(tmp){
            a->digits = tmp;
        }
    }
}

BigInt add_main(BigInt a, BigInt b){
    BigInt result;
    unsigned int sign_a = get_sign(a), sign_b = get_sign(b);
    if(sign_a == sign_b){
        result = add_magn(a, b);
        if(sign_a == 1) set_negative_sign(&result);
    }
    else {
        int cmp = compare_abs(a, b);
        if(cmp >= 0){
            result = sub_magn(a, b);
            if(sign_a == 1) set_negative_sign(&result);
        }
        else {
            result = sub_magn(b, a);
            if(sign_b == 1) set_negative_sign(&result);
        }
    }

    normalize(&result);
    return result;
}

void add_inplace(BigInt *a, BigInt b){
    BigInt tmp = add_main(*a, b);
    deinit(a);
    *a = tmp;
}

BigInt sub_main(BigInt a, BigInt b){
    BigInt b_inv = copy(&b);
    if(get_sign(b) == 1) set_positive_sign(&b_inv);
    else set_negative_sign(&b_inv);
    BigInt result = add_main(a, b_inv);
    deinit(&b_inv);

    normalize(&result);
    return result;
}

void sub_inplace(BigInt *a, BigInt b){
    BigInt tmp = sub_main(*a, b);
    deinit(a);
    *a = tmp;
}

BigInt mul_main(BigInt a, BigInt b){
    unsigned int len_a = a.digits[0] + 1;
    unsigned int len_b = b.digits[0] + 1;

    unsigned int *temp_a = malloc(len_a * sizeof(unsigned int));
    unsigned int *temp_b = malloc(len_b * sizeof(unsigned int));

    for(int i = 0; i < len_a - 1; ++i){
        temp_a[i] = a.digits[i+1];
    }
    temp_a[len_a - 1] = get_top_abs(a);
    
    for(int i = 0; i < len_b - 1; ++i){
        temp_b[i] = b.digits[i+1];
    }
    temp_b[len_b - 1] = get_top_abs(b);

    unsigned int len_res = len_a + len_b;
    unsigned int *temp_res = calloc(len_res, sizeof(unsigned int));

    for(unsigned int i = 0; i < len_a; ++i){
        unsigned long long carry = 0;
        for(unsigned int j = 0; j < len_b; ++j){
            unsigned long long curr = temp_res[i+j] + (unsigned long long)temp_a[i] * temp_b[j] + carry;
            temp_res[i+j] = (unsigned int)curr & 0xFFFFFFFF;
            carry = curr >> 32;
        }
        temp_res[i + len_b] += (unsigned int)carry;
    }

    BigInt result;
    result.digits = malloc(len_res * sizeof(unsigned int));
    result.digits[0] = len_res - 1;
    for(unsigned int i = 1; i < len_res; ++i){
        result.digits[i] = temp_res[i - 1];
    }
    result.top_digit = temp_res[len_res - 1];
    if(result.top_digit > 0x7FFFFFFF) {
        result.digits[0]++;
        result.digits = realloc(result.digits, (result.digits[0] + 1) * sizeof(unsigned int));
        result.digits[result.digits[0]] = (unsigned int)result.top_digit;
        result.top_digit = (int)(result.top_digit >> 31);
    }

    if(get_sign(a) != get_sign(b)) set_negative_sign(&result);
    else set_positive_sign(&result);

    free(temp_a);
    free(temp_b);
    free(temp_res);

    normalize(&result);
    return result;
}

void mul_inplace(BigInt *a, BigInt b){
    BigInt tmp = mul_main(*a, b);
    deinit(a);
    *a = tmp;
}

BigInt shift(BigInt a, unsigned int k){
    if(k == 0) return copy(&a);
    if(get_top_abs(a) == 0 && a.digits[0] == 0){
        init(&a, 0);
        return a;
    }
    BigInt result;
    unsigned int old_len = a.digits[0];
    result.digits = (unsigned int*)calloc(old_len + k + 1, sizeof(unsigned int));
    if(!result.digits) exit(1);
    result.digits[0] = old_len + k;
    for(unsigned int i = 1; i <= old_len; ++i){
        result.digits[i+k] = a.digits[i];
    }
    result.digits[result.digits[0]] = (unsigned int)get_top_abs(a);
    result.top_digit = 0;
    if(get_sign(a) == 1) set_negative_sign(&result);

    normalize(&result);
    return result;
}

void split(BigInt a, unsigned int k, BigInt *high, BigInt *low){
    low->digits = (unsigned int*)calloc(k+1, sizeof(unsigned int));
    low->digits[0] = k;
    for(unsigned int i = 1; i <= k; ++i){
        if(i <= a.digits[0]) low->digits[i] = a.digits[i];
    }
    low->top_digit = 0;
    set_positive_sign(low);
    normalize(low);

    unsigned int digits_count = a.digits[0] + 1;
    if(digits_count <= k) init(high, 0);
    else{
        unsigned int high_len = digits_count - k;
        high->digits = (unsigned int*)calloc(high_len, sizeof(unsigned int));
        high->digits[0] = high_len - 1;
        for(unsigned int i = 1; i <= high->digits[0]; ++i){
            high->digits[i] = a.digits[k+i];
        }
        high->top_digit = (int)get_top_abs(a);
        set_positive_sign(high);
        normalize(high);
    }
}

BigInt big_k(BigInt a, BigInt b){
    unsigned int n = (a.digits[0] > b.digits[0] ? a.digits[0] : b.digits[0]) + 1;
    if(n < 32) return mul_main(a, b);
    
    unsigned int m = n/2;
    BigInt high_a, low_a, high_b, low_b;
    split(a, m, &high_a, &low_a);
    split(b, m, &high_b, &low_b);

    BigInt z0,z1,z2,s1,s2,p,t;
    
    z0 = big_k(low_a, low_b);
    z2 = big_k(high_a, high_b);
    s1 = add_main(low_a, high_a);
    s2 = add_main(low_b, high_b);
    
    // (low_a + high_a) * (low_b + high_b) - high_a*high_b - low_a*low_b
    p = big_k(s1, s2);
    t = sub_main(p, z2);
    z1 = sub_main(t, z0);

    // high_a*high_b*B^(2m) + z1*B^m + low_a*low_b
    BigInt z2_shift = shift(z2, 2*m);
    BigInt z1_shift = shift(z1, m);

    BigInt between_res = add_main(z2_shift, z1_shift);
    BigInt result = add_main(between_res, z0);

    deinit(&z0);
    deinit(&z1);
    deinit(&z2);
    deinit(&s1);
    deinit(&s2);
    deinit(&p);
    deinit(&t);
    deinit(&high_a);
    deinit(&high_b);
    deinit(&low_a);
    deinit(&low_b);
    deinit(&z2_shift);
    deinit(&z1_shift);
    deinit(&between_res);

    return result;
}

BigInt big_k_main(BigInt a, BigInt b){
    BigInt abs_a, abs_b;
    abs_a = copy(&a);
    abs_b = copy(&b);
    set_positive_sign(&abs_a);
    set_positive_sign(&abs_b);

    BigInt result = big_k(abs_a, abs_b);

    if(get_sign(a) != get_sign(b)) set_negative_sign(&result);
    else set_positive_sign(&result);

    deinit(&abs_a);
    deinit(&abs_b);

    normalize(&result);
    return result;
}

void big_k_inplace(BigInt *a, BigInt b){
    BigInt tmp = big_k_main(*a, b);
    deinit(a);
    *a = tmp;
}

void print_bigint(const BigInt a){
    unsigned int sign = get_sign(a);
    unsigned int abs_top = get_top_abs(a);
    unsigned int len = a.digits[0];
    printf("[%c] Top: 0x%X, Digits(%u)", 
        (sign == 1) ? '-' : '+', 
        abs_top, 
        len);
    if(len > 0){
        printf(": ");
        for(int i = a.digits[0]; i >= 1; --i){
            printf("%08X ", a.digits[i]);
        }
    }
    printf("\n");   
}

void benchmark(int num_digits){
    BigInt a, b;
    a.digits = malloc((num_digits + 1) * sizeof(unsigned int));
    b.digits = malloc((num_digits + 1) * sizeof(unsigned int));
    a.digits[0] = num_digits, b.digits[0] = num_digits;
    for(int i = 1; i <= num_digits; ++i){
        a.digits[i] = rand();
        b.digits[i] = rand(); 
    }
    a.top_digit = rand() & 0x7FFFFFFF;
    b.top_digit = rand() & 0x7FFFFFFF;

    clock_t start, end;

    // Default multiplication
    start = clock();
    BigInt result_default = mul_main(a, b);
    end = clock();
    double time_default = ((double)(end - start)) / CLOCKS_PER_SEC;
    
    // Karatsuba
    start = clock();
    BigInt result_karatsuba = big_k_main(a, b);
    end = clock();
    double time_karatsuba = ((double)(end - start)) / CLOCKS_PER_SEC;

    printf("Benchmarking:\nStandart: %.4fs\nKaratsuba: %.4fs\n", time_default, time_karatsuba);
    deinit(&a);
    deinit(&b);
    deinit(&result_default);
    deinit(&result_karatsuba);
}

void demo_first(){
    printf("Task 1 demo\n");
    BigInt a, b;
    init(&a, 10);
    init(&b, 3);
    // init(&a, 1000000000);
    // init(&b, -2000000000);


    printf("Initial values:\n");
    printf("A ");
    print_bigint(a);
    printf("B ");
    print_bigint(b);
    printf("--------\n");

    // Add
    BigInt c = add_main(a, b);
    printf("[C = A + B] ");
    print_bigint(c);

    // Sub
    BigInt d = sub_main(a, b);
    printf("[D = A - B] ");
    print_bigint(d);

    // Mul
    BigInt e = mul_main(a, b);
    printf("[E = A * B] ");
    print_bigint(e);

    // Inplace add
    BigInt copy_c = copy(&a);
    add_inplace(&copy_c, b);
    printf("[A after A += B] ");
    print_bigint(copy_c);

    // Inplace sub
    BigInt copy_d = copy(&a);
    sub_inplace(&copy_d, b);
    printf("[A after A -= B] ");
    print_bigint(copy_d);

    // Inplace mul
    BigInt copy_e = copy(&a);
    mul_inplace(&copy_e, b);
    printf("[A after A *= B] ");
    print_bigint(copy_e);

    printf("------------------------\n");
    
    deinit(&a);
    deinit(&b);
    deinit(&c);
    deinit(&d);
    deinit(&e);
    deinit(&copy_c);
    deinit(&copy_d);
    deinit(&copy_e);
}

void demo_second(){
    printf("Task 2 demo\n");
    BigInt a, b;
    init(&a, 5);
    init(&b, 2);

    printf("Initial values:\n");
    printf("A ");
    print_bigint(a);
    printf("B ");
    print_bigint(b);
    printf("--------\n");

    // Big K algorithm
    BigInt c = big_k_main(a, b);
    printf("(Karatsuba)[C = A * B] ");
    print_bigint(c);

    // Inplace Big K algorithm
    BigInt copy_c = copy(&a);
    big_k_inplace(&copy_c, b);
    printf("(Karatsuba)[A after A *= B] ");
    print_bigint(copy_c);

    // Benchmarking
    benchmark(5000);

    deinit(&a);
    deinit(&b);
    deinit(&c);
    deinit(&copy_c);
}

int main(void) {
    demo_first();
    demo_second();

    return 0;
}