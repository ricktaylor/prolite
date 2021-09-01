#include "btree.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

static const size_t c_page_size = 0x1000;
static const size_t c_data_count = (c_page_size - sizeof(struct btree_page)) / (sizeof(uint64_t) + sizeof(void*));
static const size_t c_max_degree = c_data_count - 1;

static const size_t c_leaf_min = (c_max_degree + 1) / 2;
static const size_t c_internal_min = c_leaf_min - 1;

static inline void** values(struct btree_page* page)
{
	return (void**)(page->m_keys + c_data_count);
}

static size_t binary_search(struct btree_page* page, uint64_t key)
{
	// Binary search keys
	size_t l = 0;
	if (page->m_count)
	{
		size_t r = page->m_count - 1;
		while (l <= r)
		{
			size_t m = l + (r - l + 1)/2;
			if (page->m_keys[m] == key)
			{
				if (page->m_internal)
					++m;

				return m;
			}

			if (page->m_keys[m] < key)
				l = m + 1;
			else if (m == 0)
				break;
			else
				r = m - 1;
		}
	}

	return l;
}

void* btree_lookup(btree_t* bt, uint64_t key)
{
	if (!bt || !bt->m_root)
		return NULL;

	for (struct btree_page* page = bt->m_root;;)
	{
		size_t i = binary_search(page,key);
		if (!page->m_internal)
		{
			if (i > page->m_count || page->m_keys[i] != key)
				return NULL;

			return values(page)[i];
		}

		page = values(page)[i];
	}
}

static struct btree_page* new_root(btree_t* bt, struct btree_page* left, struct btree_page* right, uint64_t key)
{
	struct btree_page* new_root = (*bt->m_fn_malloc)(c_page_size);
	if (new_root)
	{
		new_root->m_internal = 1;
		new_root->m_count = 1;
		new_root->m_keys[0] = key;

		values(new_root)[0] = left;
		values(new_root)[1] = right;

		bt->m_root = new_root;
	}
	return new_root;
}

static struct btree_page* split_leaf(btree_t* bt, struct btree_page* left)
{
	struct btree_page* right = (*bt->m_fn_malloc)(c_page_size);
	if (right)
	{
		right->m_internal = 0;
		right->m_count = (c_max_degree / 2) + 1;
		left->m_count = c_max_degree + 1 - right->m_count;

		memcpy(right->m_keys,left->m_keys + left->m_count,right->m_count * sizeof(uint64_t));
		memcpy(values(right),values(left) + left->m_count,right->m_count * sizeof(void*));
	}
	return right;
}

static struct btree_page* split_internal(btree_t* bt, struct btree_page* left)
{
	struct btree_page* right = (*bt->m_fn_malloc)(c_page_size);
	if (right)
	{
		right->m_internal = 1;
		right->m_count = c_max_degree / 2;
		left->m_count = c_max_degree - right->m_count;

		memcpy(right->m_keys,left->m_keys + left->m_count,right->m_count * sizeof(uint64_t));
		memcpy(values(right),values(left) + left->m_count,(right->m_count + 1) * sizeof(void*));

		--left->m_count;
	}
	return right;
}

static struct btree_page* split_root(btree_t* bt, struct btree_page* page)
{
	struct btree_page* right = split_internal(bt,page);
	if (right)
	{
		if (!new_root(bt,page,right,page->m_keys[page->m_count]))
		{
			(*bt->m_fn_free)(right);
			right = NULL;
		}
		else
			page->m_internal = 1;
	}
	return right;
}

static int insert_internal(btree_t* bt, struct btree_page* page, struct btree_page* sub_page, uint64_t key)
{
	if (page->m_count == c_max_degree && page == bt->m_root)
	{
		struct btree_page* right = split_root(bt,page);
		if (!right)
			return 0;

		if (key >= page->m_keys[page->m_count])
			page = right;
	}

	size_t i = binary_search(page,key);
	if (i < page->m_count)
	{
		memmove(page->m_keys + i+1,page->m_keys + i,(page->m_count - i) * sizeof(uint64_t));
		memmove(values(page) + i+2,values(page) + i+1,(page->m_count - i) * sizeof(void*));
	}

	page->m_keys[i] = key;
	values(page)[i+1] = sub_page;
	++page->m_count;
	return 1;
}

static void* kv_insert(btree_t* bt, struct btree_page** page, uint64_t key, void* val)
{
	size_t i = binary_search(*page,key);
	if (!(*page)->m_internal)
	{
		if (i < (*page)->m_count && (*page)->m_keys[i] == key)
			return values(*page)[i];

		if ((*page)->m_count == c_max_degree + 1)
		{
			// We're full, a previous alloc must have failed
			return NULL;
		}

		if (i < (*page)->m_count)
		{
			memmove((*page)->m_keys + i+1,(*page)->m_keys + i,((*page)->m_count - i) * sizeof(uint64_t));
			memmove(values(*page) + i+1,values(*page) + i,((*page)->m_count - i) * sizeof(void*));
		}

		(*page)->m_keys[i] = key;
		values(*page)[i] = val;
		++(*page)->m_count;

		if ((*page)->m_count == c_max_degree + 1 && *page == bt->m_root)
		{
			struct btree_page* right = split_leaf(bt,*page);
			if (!right)
				return NULL;

			if (!new_root(bt,*page,right,right->m_keys[0]))
			{
				(*bt->m_fn_free)(right);
				(*page)->m_count = c_max_degree + 1;
				return NULL;
			}

			if (i >= (*page)->m_count)
				*page = right;
		}
	}
	else
	{
		struct btree_page* sub_page = values(*page)[i];
		val = kv_insert(bt,&sub_page,key,val);
		if (val)
		{
			if (!sub_page->m_internal)
			{
				if (sub_page->m_count == c_max_degree + 1)
				{
					struct btree_page* sub_right = split_leaf(bt,sub_page);
					if (sub_right && !insert_internal(bt,*page,sub_right,sub_right->m_keys[0]))
						sub_page->m_count = c_max_degree + 1;
				}
			}
			else if (sub_page->m_count == c_max_degree)
			{
				struct btree_page* sub_right = split_internal(bt,sub_page);
				if (sub_right && !insert_internal(bt,*page,sub_right,sub_page->m_keys[sub_page->m_count]))
					sub_page->m_count = c_max_degree + 1;
			}
		}
	}

	return val;
}

void* btree_insert(btree_t* bt, uint64_t key, void* val)
{
	if (!bt)
		return NULL;

	if (!bt->m_root)
	{
		bt->m_root = (*bt->m_fn_malloc)(c_page_size);
		if (!bt->m_root)
			return NULL;

		bt->m_root->m_internal = 0;
		bt->m_root->m_count = 0;
	}

	struct btree_page* page = bt->m_root;
	return kv_insert(bt,&page,key,val);
}

static void* kv_remove(btree_t* bt, struct btree_page* page, uint64_t key)
{
	void* val = NULL;
	size_t i = binary_search(page,key);
	if (!page->m_internal)
	{
		if (i < page->m_count && page->m_keys[i] == key)
		{
			val = values(page)[i];

			if (i < page->m_count - 1)
			{
				memmove(page->m_keys + i,page->m_keys + i+1,(page->m_count - i - 1) * sizeof(uint64_t));
				memmove(values(page) + i,values(page) + i+1,(page->m_count - i - 1) * sizeof(void*));
			}
			--page->m_count;
		}
	}
	else
	{
		struct btree_page* sub_page = values(page)[i];
		if ((val = kv_remove(bt,sub_page,key)))
		{
			if (!sub_page->m_internal)
			{
				if (sub_page->m_count < c_leaf_min)
				{
					if (i > 0 && ((struct btree_page*)values(page)[i-1])->m_count > c_leaf_min)
					{
						struct btree_page* left = values(page)[i-1];
						size_t t = (left->m_count - sub_page->m_count) / 2;
						memmove(sub_page->m_keys + t,sub_page->m_keys,sub_page->m_count * sizeof(uint64_t));
						memmove(values(sub_page) + t,values(sub_page),sub_page->m_count * sizeof(void*));
						memcpy(sub_page->m_keys,left->m_keys + (left->m_count - t),t * sizeof(uint64_t));
						memcpy(values(sub_page),values(left) + (left->m_count - t),t * sizeof(void*));
						sub_page->m_count += t;
						left->m_count -= t;
						page->m_keys[i-1] = sub_page->m_keys[0];
					}
					else if (i < page->m_count && ((struct btree_page*)values(page)[i+1])->m_count > c_leaf_min)
					{
						struct btree_page* right = values(page)[i+1];
						size_t t = (right->m_count - sub_page->m_count) / 2;
						memcpy(sub_page->m_keys + sub_page->m_count,right->m_keys,t * sizeof(uint64_t));
						memcpy(values(sub_page) + sub_page->m_count,values(right),t * sizeof(void*));
						memmove(right->m_keys,right->m_keys + t,(right->m_count - t) * sizeof(uint64_t));
						memmove(values(right),values(right) + t,(right->m_count - t) * sizeof(void*));
						sub_page->m_count += t;
						right->m_count -= t;
						page->m_keys[i] = right->m_keys[0];
					}
					else
					{
						if (i > 0)
						{
							struct btree_page* left = values(page)[i-1];
							memcpy(left->m_keys + left->m_count,sub_page->m_keys,sub_page->m_count * sizeof(uint64_t));
							memcpy(values(left) + left->m_count,values(sub_page),sub_page->m_count * sizeof(void*));
							left->m_count += sub_page->m_count;
							page->m_keys[i-1] = page->m_keys[i];
						}
						else
						{
							struct btree_page* right = values(page)[1];
							memmove(right->m_keys + sub_page->m_count,right->m_keys,right->m_count * sizeof(uint64_t));
							memmove(values(right) + sub_page->m_count,values(right),right->m_count * sizeof(void*));
							memcpy(right->m_keys,sub_page->m_keys,sub_page->m_count * sizeof(uint64_t));
							memcpy(values(right),values(sub_page),sub_page->m_count * sizeof(void*));
							right->m_count += sub_page->m_count;
						}

						if (i < page->m_count)
						{
							memmove(page->m_keys + i,page->m_keys + i+1,(page->m_count - i - 1) * sizeof(uint64_t));
							memmove(values(page) + i,values(page) + i+1,(page->m_count - i) * sizeof(void*));
						}
						--page->m_count;

						(*bt->m_fn_free)(sub_page);

						if (page == bt->m_root && page->m_count == 0)
						{
							bt->m_root = values(page)[0];
							(*bt->m_fn_free)(page);
						}
					}
				}
				else if (i > 0)
					page->m_keys[i-1] = sub_page->m_keys[0];
			}
			else if (sub_page->m_count < c_internal_min)
			{
				if (i > 0 && ((struct btree_page*)values(page)[i-1])->m_count > c_internal_min)
				{
					struct btree_page* left = values(page)[i-1];
					size_t t = (left->m_count - sub_page->m_count) / 2;
					memmove(sub_page->m_keys + t,sub_page->m_keys,sub_page->m_count * sizeof(uint64_t));
					memmove(values(sub_page) + t,values(sub_page),(sub_page->m_count + 1) * sizeof(void*));
					sub_page->m_keys[t-1] = page->m_keys[i-1];
					memcpy(sub_page->m_keys,left->m_keys + (left->m_count - t) + 1,(t - 1) * sizeof(uint64_t));
					page->m_keys[i-1] = left->m_keys[left->m_count - t];
					memcpy(values(sub_page),values(left) + (left->m_count - t) + 1,t * sizeof(void*));
					sub_page->m_count += t;
					left->m_count -= t;
				}
				else if (i < page->m_count && ((struct btree_page*)values(page)[i+1])->m_count > c_internal_min)
				{
					struct btree_page* right = values(page)[i+1];
					size_t t = (right->m_count - sub_page->m_count) / 2;
					sub_page->m_keys[sub_page->m_count] = page->m_keys[i];
					page->m_keys[i] = right->m_keys[t-1];
					memcpy(sub_page->m_keys + sub_page->m_count + 1,right->m_keys,(t - 1) * sizeof(uint64_t));
					memcpy(values(sub_page) + sub_page->m_count + 1,values(right),t * sizeof(void*));
					memmove(right->m_keys,right->m_keys + t,(right->m_count - t) * sizeof(uint64_t));
					memmove(values(right),values(right) + t,(right->m_count - t + 1) * sizeof(void*));
					sub_page->m_count += t;
					right->m_count -= t;
				}
				else
				{
					if (i > 0)
					{
						struct btree_page* left = values(page)[i-1];
						left->m_keys[left->m_count] = page->m_keys[i-1];
						memcpy(left->m_keys + left->m_count + 1,sub_page->m_keys,sub_page->m_count * sizeof(uint64_t));
						memcpy(values(left) + left->m_count + 1,values(sub_page),(sub_page->m_count + 1) * sizeof(void*));
						left->m_count += (sub_page->m_count+1);
						page->m_keys[i-1] = page->m_keys[i];
					}
					else
					{
						struct btree_page* right = values(page)[1];
						memmove(right->m_keys + sub_page->m_count + 1,right->m_keys,right->m_count * sizeof(uint64_t));
						memmove(values(right) + sub_page->m_count + 1,values(right),(right->m_count + 1) * sizeof(void*));
						right->m_keys[sub_page->m_count] = page->m_keys[0];
						memcpy(right->m_keys,sub_page->m_keys,sub_page->m_count * sizeof(uint64_t));
						memcpy(values(right),values(sub_page),(sub_page->m_count + 1) * sizeof(uint64_t));
						right->m_count += (sub_page->m_count+1);
					}

					if (i < page->m_count)
					{
						memmove(page->m_keys + i,page->m_keys + i+1,(page->m_count - i - 1) * sizeof(uint64_t));
						memmove(values(page) + i,values(page) + i+1,(page->m_count - i) * sizeof(void*));
					}
					--page->m_count;

					(*bt->m_fn_free)(sub_page);

					if (page == bt->m_root && page->m_count == 0)
					{
						bt->m_root = values(page)[0];
						(*bt->m_fn_free)(page);
					}
				}
			}
		}
	}

	return val;
}

void* btree_remove(btree_t* bt, uint64_t key)
{
	if (!bt || !bt->m_root)
		return NULL;

	return kv_remove(bt,bt->m_root,key);
}

#if ENABLE_TESTS
#include <stdio.h>

static void dump_page(FILE* f, struct btree_page* page)
{
	if (page)
	{
		fprintf(f,"\tpage%p [label=\"",page);

		if (!page->m_internal)
		{
			for (size_t i=0; i < page->m_count; ++i)
			{
				if (i)
					fprintf(f,"|");

				fprintf(f,"%zu",(size_t)page->m_keys[i]);
			}

			fprintf(f,"\"];\n");
		}
		else
		{
			size_t i;
			for (i=0; i < page->m_count; ++i)
			{
				if (i)
					fprintf(f,"|");

				fprintf(f,"<p%zu>.|%zu",i,(size_t)page->m_keys[i]);
			}
			fprintf(f,"|<p%zu>.\"];\n",i);

			for (i=0; i < page->m_count; ++i)
				dump_page(f,values(page)[i]);
			dump_page(f,values(page)[i]);

			for (i=0; i < page->m_count; ++i)
			{
				if (values(page)[i])
					fprintf(f,"\tpage%p:p%zu -> page%p;\n",page,i,values(page)[i]);
			}

			if (values(page)[i])
				fprintf(f,"\tpage%p:p%zu -> page%p;\n",page,i,values(page)[i]);
		}
	}
	else
		fprintf(f,"\tpage0 [label=\"NULL\"];\n");
}

static void dump_btree(const btree_t* bt, const char* filename)
{
	FILE* f = fopen(filename,"w");

	fprintf(f,"digraph cfg {\n\tnode [shape=record];\n");

	dump_page(f,bt->m_root);

	fprintf(f,"}");

	fclose(f);
}

void btree_tests(void)
{
	btree_t bt = { .m_fn_malloc = &malloc, .m_fn_free = &free };

	int v = 12;
	for (size_t i=0; i < 5000; ++i)
	{
		btree_insert(&bt,rand() % 999 + 1,&v);

		dump_btree(&bt,"./btree.dot");

		btree_remove(&bt,rand() % 999 + 1);

		dump_btree(&bt,"./btree.dot");
	}
}
#endif // ENABLE_TESTS
