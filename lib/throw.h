/*
 * throw.h
 *
 *  Created on: 16 May 2017
 *      Author: rick
 */

#ifndef THROW_H_
#define THROW_H_

#include "types.h"

int throw_instantiation_error(struct context_t* context);

int throw_type_error(struct context_t* context, uint64_t valid_type_atom, const union box_t* culprit);

int throw_domain_error(struct context_t* context, uint64_t valid_domain_atom, const union box_t* culprit);

int throw_existence_error(struct context_t* context, uint64_t object_atom, const union box_t* culprit);

int throw_permission_error(struct context_t* context, uint64_t operation_atom, uint64_t permission_atom, const union box_t* culprit);

int throw_representation_error(struct context_t* context, uint64_t flag_atom);

int throw_evaluation_error(struct context_t* context, uint64_t error_atom);

int throw_oom_error(struct context_t* context);

#endif /* THROW_H_ */
