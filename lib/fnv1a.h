
// This code is derived from the original FNV1a code from Landon Curt Noll

/*
 *
 * Please do not copyright this code.  This code is in the public domain.
 *
 * LANDON CURT NOLL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
 * EVENT SHALL LANDON CURT NOLL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * By:
 *	chongo <Landon Curt Noll> /\oo/\
 *      http://www.isthe.com/chongo/
 *
 * Share and Enjoy!	:-)
 */

#ifndef FNV1A_H_
#define FNV1A_H_

#include <stdint.h>
#include <stddef.h>

static inline uint64_t fnv1a_64(const unsigned char* buf, size_t len)
{
	uint64_t hval = UINT64_C(0xcbf29ce484222325);

	while (len--)
	{
		/* xor the bottom with the current octet */
		hval ^= (uint64_t)*buf++;

		/* multiply by the 64 bit FNV magic prime mod 2^64 */
		//hval *= FNV_64_PRIME;
		hval += (hval << 1) + (hval << 4) + (hval << 5) + (hval << 7) + (hval << 8) + (hval << 40);
	}

	/* return our new hash value */
	return hval;
}

#endif // FNV1A_H_
