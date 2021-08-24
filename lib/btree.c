
#include "btree.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

static const size_t c_page_size = 0x1000;
static const size_t c_page_max_count = 3;//(c_page_size - sizeof(struct btree_page)) / sizeof(struct btree_kv);

static size_t kv_search(struct btree_page* page, uint64_t key)
{
	// Binary search keys
	size_t l = 0;
	if (page->m_count)
	{
		size_t r = page->m_count - 1;
		while (l <= r)
		{
			size_t m = l + (r - l + 1)/2;
			if (page->m_data[m].key == key)
				return m;

			if (page->m_data[m].key < key)
				l = m + 1;
			else if (m == 0)
				break;
			else	
				r = m - 1;
		}
	}

	return l;
}

static size_t kv_lookup(struct btree_page** page, uint64_t key)
{
	for (;;)
	{
		size_t i = kv_search(*page,key);
		if ((*page)->m_type == bt_leaf)
			return i;
				
		*page = (*page)->m_data[i].value;
	}
}

void* btree_lookup(btree_t* bt, uint64_t key)
{
	if (!bt || !bt->m_root)
		return NULL;

	struct btree_page* page = bt->m_root;
	size_t i = kv_lookup(&page,key);
	if (i > page->m_count || page->m_data[i].key != key)
		return NULL;
	
	return page->m_data[i].value;
}

static struct btree_page* new_page(btree_t* bt, btree_page_type_t type)
{
	struct btree_page* page = (*bt->m_fn_malloc)(c_page_size);
	if (page)
	{
		memset(page,0,c_page_size);
		page->m_type = type;
	}
	return page;
}

static int insert_page(btree_t* bt, struct btree_page* page, uint64_t key, struct btree_page* sub_page)
{
	assert(page->m_type != bt_leaf);

	if (page->m_count == c_page_max_count - 1)
	{
		struct btree_page* left = page;
		struct btree_page* right = new_page(bt,bt_internal);
		if (!right)
			return 0;

		size_t split = c_page_max_count / 2;
		right->m_count = (left->m_count - split) - 1;
		
		memcpy(right->m_data,left->m_data + split + 1,(right->m_count + 1) * sizeof(left->m_data[0]));
		
		if (left == bt->m_root)
		{
			struct btree_page* new_root = new_page(bt,bt_root);
			if (!new_root)
			{
				(*bt->m_fn_free)(right);
				return 0;
			}

			new_root->m_count = 1;
			new_root->m_data[0].key = left->m_data[split].key;
			new_root->m_data[0].value = left; 
			new_root->m_data[1].value = right;

			left->m_type = bt_internal;

			right->m_parent = left->m_parent = bt->m_root = new_root;
		}
		else
		{
			right->m_parent = left->m_parent;
			if (!insert_page(bt,left->m_parent,left->m_data[split].key,right))
			{
				(*bt->m_fn_free)(right);
				return 0;
			}
		}

		// Reparent children
		for (size_t i = 0; i <= right->m_count; ++i)
			((struct btree_page*)right->m_data[i].value)->m_parent = right;
		
		left->m_count = split;		
		if (key >= left->m_data[split].key)
			page = right;
	}
	
	size_t i = kv_search(page,key);
	if (i == page->m_count)
	{
		page->m_data[i].key = key;
		page->m_data[i+1].value = sub_page;
	}
	else
	{
		memmove(page->m_data + i+1,page->m_data + i,(page->m_count - i + 1) * sizeof(page->m_data[0]));
		page->m_data[i].key = key;
		page->m_data[i+1].value = sub_page;
	}

	++page->m_count;
	sub_page->m_parent = page;
	
	return 1;	
}

void* btree_insert(btree_t* bt, uint64_t key, void* val)
{
	if (!bt)
		return NULL;

	if (!bt->m_root)
	{
		bt->m_root = new_page(bt,bt_leaf);
		if (!bt->m_root)
			return NULL;
	}

	struct btree_page* page = bt->m_root;

	size_t i = kv_lookup(&page,key);
	if (i < page->m_count && page->m_data[i].key == key)
		return page->m_data[i].value;

	if (page->m_count == c_page_max_count)
	{
		struct btree_page* left = page;
		struct btree_page* right = new_page(bt,bt_leaf);
		if (!right)
			return NULL;

		right->m_count = (c_page_max_count + 1) / 2;
		
		memcpy(right->m_data,left->m_data + (left->m_count - right->m_count),right->m_count * sizeof(left->m_data[0]));

		if (left == bt->m_root)
		{
			struct btree_page* new_root = new_page(bt,bt_root);
			if (!new_root)
			{
				(*bt->m_fn_free)(right);
				return NULL;
			}

			new_root->m_count = 1;
			new_root->m_data[0].key = right->m_data[0].key;
			new_root->m_data[0].value = left; 
			new_root->m_data[1].value = right;

			right->m_parent = left->m_parent = bt->m_root = new_root;
		}
		else
		{
			right->m_parent = left->m_parent;
			if (!insert_page(bt,left->m_parent,right->m_data[0].key,right))
			{
				(*bt->m_fn_free)(right);
				return NULL;
			}
		}
		
		left->m_count -= right->m_count;
		if (i > left->m_count)
		{
			page = right;
			i = kv_search(page,key);
		}
	}

	if (i == page->m_count)
	{
		page->m_data[i].key = key;
		page->m_data[i].value = val;
	}
	else
	{
		memmove(page->m_data + i+1,page->m_data + i,(page->m_count - i) * sizeof(page->m_data[0]));
		page->m_data[i].key = key;
		page->m_data[i].value = val;
	}
	
	++page->m_count;
	
	return val;	
}

#include <stdio.h>

static void dump_page(FILE* f, struct btree_page* page)
{
	fprintf(f,"\tpage%p [label=\"",page);

	if (page->m_type == bt_leaf)
	{		
		for (size_t i=0; i < page->m_count; ++i)
		{
			if (i)
				fprintf(f,"|");

			fprintf(f,"%zu",(size_t)page->m_data[i].key);
		}

		fprintf(f,"\"];\n");
	}
	else
	{
		for (size_t i=0; i < page->m_count; ++i)
		{
			if (i)
				fprintf(f,"|");
		
			fprintf(f,"<p%zu>.|%zu",i,(size_t)page->m_data[i].key);	
		}
		
		fprintf(f,"|<p%zu>.\"];\n",page->m_count);

		for (size_t i=0; i < page->m_count; ++i)
			dump_page(f,page->m_data[i].value);

		dump_page(f,page->m_data[page->m_count].value);
		
		for (size_t i=0; i < page->m_count; ++i)
			fprintf(f,"\tpage%p:p%zu -> page%p;\n",page,i,page->m_data[i].value);
		
		fprintf(f,"\tpage%p:p%zu -> page%p;\n",page,page->m_count,page->m_data[page->m_count].value);
	}	
}

void dump_btree(const btree_t* bt, const char* filename)
{
	FILE* f = fopen(filename,"w");

	fprintf(f,"digraph cfg {\n\tnode [shape=record];\n");

	dump_page(f,bt->m_root);
	
	fprintf(f,"}");

	fclose(f);
}
