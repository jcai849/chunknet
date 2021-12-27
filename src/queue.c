#include <stdlib.h>
#include <Rinternals.h>
#include "queue.h"

typedef struct Node_t {
    struct Node_t *next;
    SEXP val;
} Node;

typedef struct Queue_t {
    Node *front;
    Node *back;
} Queue;

SEXP C_queue()
{
    SEXP R_queue;
    Queue *queue;

    queue = (Queue *) malloc(sizeof(Queue));
    queue->front = NULL;
    queue->back = NULL;
    R_queue = R_MakeExternalPtr(queue, R_NilValue, R_NilValue);
    return R_queue;
}

SEXP C_push(SEXP queue, SEXP val)
{
    Queue *pqueue;
    Node *node;

    R_PreserveObject(val);
    pqueue = R_ExternalPtrAddr(queue);
    node = malloc(sizeof(Node));
    node->next = NULL;
    node->val = val;

    if (pqueue->front == NULL) {
        pqueue->front = node;
    } else {
        pqueue->back->next = node;
    }
    pqueue->back = node;

    return ScalarLogical(1);
}

SEXP C_pop(SEXP queue)
{
    Queue *pqueue;
    Node *next_node;
    SEXP val;

    pqueue = R_ExternalPtrAddr(queue);
    if (pqueue->front == NULL)
        return R_NilValue;
    val = pqueue->front->val;
    next_node = pqueue->front;
    pqueue->front = next_node->next;
    free(next_node);
    return val;
}
