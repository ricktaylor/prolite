#include "btree.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

static const size_t c_page_size = 0x100;
static const size_t c_max_items = (c_page_size - sizeof(struct btree_page)) / (sizeof(uint64_t) + sizeof(void*));

static inline void** values(struct btree_page* page)
{
	return (void**)(page->m_keys + c_max_items);
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

static struct btree_page* split_leaf(btree_t* bt, struct btree_page* left)
{
	struct btree_page* right = allocator_malloc(bt->m_allocator,c_page_size);
	if (right)
	{
		right->m_internal = 0;
		right->m_count = ((c_max_items - 1) / 2) + 1;
		left->m_count = c_max_items - right->m_count;

		memcpy(right->m_keys,left->m_keys + left->m_count,right->m_count * sizeof(uint64_t));
		memcpy(values(right),values(left) + left->m_count,right->m_count * sizeof(void*));
	}
	return right;
}

static struct btree_page* split_internal(btree_t* bt, struct btree_page* left)
{
	struct btree_page* right = allocator_malloc(bt->m_allocator,c_page_size);
	if (right)
	{
		right->m_internal = 1;
		right->m_count = (c_max_items - 1) / 2;
		left->m_count = c_max_items - 1 - right->m_count;

		memcpy(right->m_keys,left->m_keys + left->m_count,right->m_count * sizeof(uint64_t));
		memcpy(values(right),values(left) + left->m_count,(right->m_count + 1) * sizeof(void*));

		--left->m_count;
	}
	return right;
}

static void copy_page(struct btree_page* dst, const struct btree_page* src)
{
	void** src_values = (void**)(src->m_keys + c_max_items);

	memcpy(dst->m_keys,src->m_keys,src->m_count * sizeof(uint64_t));
	memcpy(values(dst),src_values,src->m_count * sizeof(void*));

	if (src->m_internal)
		values(dst)[src->m_count] = src_values[src->m_count];
	
	dst->m_internal = src->m_internal;
	dst->m_count = src->m_count;
}

static struct btree_page* split_root(btree_t* bt, uint64_t key)
{
	// We need to keep bt->m_root stable
	struct btree_page* left = allocator_malloc(bt->m_allocator,c_page_size);
	if (!left)
		return NULL;

	struct btree_page* right = allocator_malloc(bt->m_allocator,c_page_size);
	if (!right)
	{
		allocator_free(bt->m_allocator,left);
		return NULL;
	}

	right->m_internal = left->m_internal = bt->m_root->m_internal;

	if (bt->m_root->m_internal)
	{
		right->m_count = (c_max_items - 1) / 2;
		left->m_count = c_max_items - 1 - right->m_count;

		memcpy(right->m_keys,bt->m_root->m_keys + left->m_count,right->m_count * sizeof(uint64_t));
		memcpy(values(left),values(bt->m_root),left->m_count * sizeof(void*));
		memcpy(values(right),values(bt->m_root) + left->m_count,(right->m_count + 1) * sizeof(void*));

		--left->m_count;
	}
	else
	{
		right->m_count = ((c_max_items - 1) / 2) + 1;
		left->m_count = c_max_items - right->m_count;
		
		memcpy(right->m_keys,bt->m_root->m_keys + left->m_count,right->m_count * sizeof(uint64_t));
		memcpy(values(left),values(bt->m_root),left->m_count * sizeof(void*));
		memcpy(values(right),values(bt->m_root) + left->m_count,right->m_count * sizeof(void*));
	}

	memcpy(left->m_keys,bt->m_root->m_keys,left->m_count * sizeof(uint64_t));

	bt->m_root->m_keys[0] = bt->m_root->m_keys[left->m_count];
	bt->m_root->m_internal = 1;
	bt->m_root->m_count = 1;

	values(bt->m_root)[0] = left;
	values(bt->m_root)[1] = right;

	return (key >= bt->m_root->m_keys[0]) ? right : left;
}

static int insert_internal(btree_t* bt, struct btree_page* page, struct btree_page* sub_page, uint64_t key)
{
	if (page->m_count == c_max_items - 1 && page == bt->m_root)
	{
		page = split_root(bt,key);
		if (!page)
			return 0;
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

		if ((*page)->m_count == c_max_items)
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

		if ((*page)->m_count == c_max_items && *page == bt->m_root)
		{
			*page = split_root(bt,key);
			if (!*page)
				return NULL;
		}
	}
	else
	{
		struct btree_page* sub_page = values(*page)[i];
		val = kv_insert(bt,&sub_page,key,val);
		if (val)
		{
			uint32_t prev_count = sub_page->m_count;
			struct btree_page* sub_right = NULL;
			if (!sub_page->m_internal)
			{
				if (sub_page->m_count == c_max_items)
					sub_right = split_leaf(bt,sub_page);
			}
			else if (sub_page->m_count == c_max_items - 1)
				sub_right = split_internal(bt,sub_page);

			if (sub_right && !insert_internal(bt,*page,sub_right,sub_page->m_keys[sub_page->m_count]))
				sub_page->m_count = prev_count;
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
		bt->m_root = allocator_malloc(bt->m_allocator,c_page_size);
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
				if (sub_page->m_count < c_max_items / 2)
				{
					if (i > 0 && ((struct btree_page*)values(page)[i-1])->m_count > c_max_items / 2)
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
					else if (i < page->m_count && ((struct btree_page*)values(page)[i+1])->m_count > c_max_items / 2)
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

						allocator_free(bt->m_allocator,sub_page);

						if (page == bt->m_root && page->m_count == 0)
						{
							sub_page = values(page)[0];
							copy_page(bt->m_root,sub_page);
							allocator_free(bt->m_allocator,sub_page);
						}
					}
				}
				else if (i > 0)
					page->m_keys[i-1] = sub_page->m_keys[0];
			}
			else if (sub_page->m_count < (c_max_items / 2) - 1)
			{
				if (i > 0 && ((struct btree_page*)values(page)[i-1])->m_count > (c_max_items / 2) - 1)
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
				else if (i < page->m_count && ((struct btree_page*)values(page)[i+1])->m_count > (c_max_items / 2) - 1)
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

					allocator_free(bt->m_allocator,sub_page);

					if (page == bt->m_root && page->m_count == 0)
					{
						sub_page = values(page)[0];
						copy_page(bt->m_root,sub_page);
						allocator_free(bt->m_allocator,sub_page);
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

static void page_clear(btree_t* bt, struct btree_page* page, void (*callback)(void* param, uint64_t key, void* val), void* param)
{
	if (page->m_internal)
	{
		size_t i;
		for (i=0; i < page->m_count; ++i)
			page_clear(bt,values(page)[i],callback,param);
		page_clear(bt,values(page)[i],callback,param);
	}
	else if (callback)
	{
		for (size_t i=0; i < page->m_count; ++i)
			(*callback)(param,page->m_keys[i],values(page)[i]);
	}
	
	allocator_free(bt->m_allocator,page);
}

void btree_clear(btree_t* bt, void (*callback)(void* param, uint64_t key, void* val), void* param)
{
	if (bt && bt->m_root)
	{
		page_clear(bt,bt->m_root,callback,param);
		bt->m_root = NULL;
	}
}

static void page_enum(btree_t* bt, struct btree_page* page, void (*callback)(void* param, uint64_t key, void* val), void* param)
{
	if (page->m_internal)
	{
		size_t i;
		for (i=0; i < page->m_count; ++i)
			page_enum(bt,values(page)[i],callback,param);
		page_enum(bt,values(page)[i],callback,param);
	}
	else
	{
		for (size_t i=0; i < page->m_count; ++i)
			(*callback)(param,page->m_keys[i],values(page)[i]);
	}
}

void btree_enum(btree_t* bt, void (*callback)(void* param, uint64_t key, void* val), void* param)
{
	assert(callback);

	if (bt && bt->m_root)
		page_enum(bt,bt->m_root,callback,param);
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
	btree_t bt = {0};

	int v = 12;
	for (size_t i=0; i < 5000; ++i)
	{
		btree_insert(&bt,rand() % 999 + 1,&v);

		dump_btree(&bt,"./btree.dot");

		btree_remove(&bt,rand() % 999 + 1);

		dump_btree(&bt,"./btree.dot");
	}

	btree_clear(&bt,NULL,NULL);
}
#endif // ENABLE_TESTS
