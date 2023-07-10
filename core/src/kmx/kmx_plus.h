/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295
  This file defines the structure of a kmx_plus file, starting at COMP_KEYBOARD_KMXPLUSINFO.dpKMXPlus
*/

#pragma once

#include <string>
#include <km_types.h>
#include <kmx/kmx_base.h>
#include <kmx_file.h>
#include <ldml/keyboardprocessor_ldml.h>

namespace km {
namespace kbp {
namespace kmx {

/**
 * Using C99 flexible array initializers: entries[]
 * https://docs.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warning-levels-2-and-4-c4200
 */
#if defined(_WIN32)
#pragma warning ( disable : 4200 )
#endif

/**
 * Indicates an offset into the strs table (0 = zero length)
 */
typedef KMX_DWORD KMXPLUS_STR;
/**
 * Indicates an offset into the list table (0 = zero length)
*/
typedef KMX_DWORD KMXPLUS_LIST;
/**
 * Indicates an offset into the elem table (0 = zero length)
*/
typedef KMX_DWORD KMXPLUS_ELEM;

// forward declarations
struct COMP_KMXPLUS_TRAN;
struct COMP_KMXPLUS_TRAN_GROUP;
struct COMP_KMXPLUS_TRAN_TRANSFORM;
struct COMP_KMXPLUS_TRAN_REORDER;

struct COMP_KMXPLUS_HEADER {
  KMX_DWORD ident;  // 0000 Section name
  KMX_DWORD size;   // 0004 Section length
  bool valid(KMX_DWORD length) const;
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_HEADER) == LDML_LENGTH_HEADER, "mismatched size of section header");

/* ------------------------------------------------------------------
 * sect section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_SECT_ENTRY {
  KMX_DWORD sect;    // 0010+ Section identity
  KMX_DWORD offset;  // 0014+ Section offset relative to dpKMXPlus of section
};

struct COMP_KMXPLUS_SECT {
  static const KMX_DWORD IDENT = LDML_SECTIONID_SECT;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD total;                     // 0008 KMXPlus entire length
  KMX_DWORD count;                     // 000C number of section headers
  COMP_KMXPLUS_SECT_ENTRY entries[];   // 0010 section entries
  /**
   * @brief Get the offset of a section, or 0
   *
   * @param ident section id such as 'strs'. Never 'sect'
   * @return KMX_DWORD offset from beginning of kmxplus
   */
  KMX_DWORD find(KMX_DWORD ident) const;
  /**
   * @brief True if section is valid.
   * Does not validate the entire file.
   */
  bool valid(KMX_DWORD length) const;
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_SECT) == LDML_LENGTH_SECT, "mismatched size of section sect");
static_assert(sizeof(struct COMP_KMXPLUS_SECT) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");

/* ------------------------------------------------------------------
 * elem section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_ELEM_ELEMENT {
    KMX_DWORD element;                // str: output string or UTF-32LE codepoint
    KMX_DWORD flags;                  // flag and order values
    /**
     * @brief Get the 'element' as a string, if flags&LDML_ELEM_FLAGS_TYPE = CHAR
     *
     * @return std::u16string
     */
    std::u16string get_element_string() const;
};

struct COMP_KMXPLUS_ELEM_ENTRY {
    KMX_DWORD offset;                 // 0010+ offset from this blob
    KMX_DWORD length;                 // 0014+ str length (ELEMENT units)
};

struct COMP_KMXPLUS_ELEM {
  static const KMX_DWORD IDENT = LDML_SECTIONID_ELEM;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;                    // 0008 count of str entries
  COMP_KMXPLUS_ELEM_ENTRY entries[];  // 000C+ entries

  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
  /**
   * @param elementNumber element number, 0..count-1
   * @param length fillin: length of list
   * @return pointer to first element of list of length length. or nullptr
   */
  const COMP_KMXPLUS_ELEM_ELEMENT *getElementList(KMX_DWORD elementNumber,
                                                  KMX_DWORD &length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_ELEM) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_ELEM) == LDML_LENGTH_ELEM, "mismatched size of section elem");

/* ------------------------------------------------------------------
 * finl section is no more
   ------------------------------------------------------------------ */

/* ------------------------------------------------------------------
 * keys section is now key2.kmap
   ------------------------------------------------------------------ */

/* ------------------------------------------------------------------
 * loca section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_LOCA_ENTRY {
  KMXPLUS_STR locale; // 0010+ locale string entry
};

struct COMP_KMXPLUS_LOCA {
  static const KMX_DWORD IDENT = LDML_SECTIONID_LOCA;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count; // 0008 number of locales
  COMP_KMXPLUS_LOCA_ENTRY entries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_LOCA) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_LOCA) == LDML_LENGTH_LOCA, "mismatched size of section loca");

/* ------------------------------------------------------------------
 * meta section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_META {
  static const KMX_DWORD IDENT = LDML_SECTIONID_META;
  COMP_KMXPLUS_HEADER header;
  KMXPLUS_STR author;
  KMXPLUS_STR conform;
  KMXPLUS_STR layout;
  KMXPLUS_STR normalization;
  KMXPLUS_STR indicator;
  KMXPLUS_STR version;
  KMX_DWORD settings;
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_META) == LDML_LENGTH_META, "mismatched size of section meta");

/* ------------------------------------------------------------------
 * name section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_NAME_ENTRY {
    KMXPLUS_STR name;
};

struct COMP_KMXPLUS_NAME {
  static const KMX_DWORD IDENT = LDML_SECTIONID_NAME;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;
  COMP_KMXPLUS_NAME_ENTRY entries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_NAME) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_NAME) == LDML_LENGTH_NAME, "mismatched size of section name");

/* ------------------------------------------------------------------
 * strs section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_STRS_ENTRY {
    KMX_DWORD offset;                 // 0010+ offset from this blob
    KMX_DWORD length;                 // 0014+ str length (UTF-16LE units)
};

struct COMP_KMXPLUS_STRS {
  static const KMX_DWORD IDENT = LDML_SECTIONID_STRS;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;                    // 0008 count of str entries
  COMP_KMXPLUS_STRS_ENTRY entries[];  // 0010+ entries

  /**
   * @brief Get a string entry
   *
   * @param entry entry number
   * @param buf output buffer
   * @param bufsiz buffer size in bytes
   * @return nullptr or a pointer to the output buffer
   */
  std::u16string get(KMX_DWORD entry) const;

  /**
   * Slow search
   */
  KMX_DWORD find(const std::u16string&) const;
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;

  /** convert a single char to a string*/
  static std::u16string str_from_char(KMX_DWORD v);
};

static_assert(sizeof(struct COMP_KMXPLUS_STRS) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_STRS) == LDML_LENGTH_STRS, "mismatched size of section strs");

/* ------------------------------------------------------------------
 * tran section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_TRAN_GROUP {
    KMX_DWORD type;
    KMX_DWORD count;
    KMX_DWORD index;
};

struct COMP_KMXPLUS_TRAN_TRANSFORM {
    KMXPLUS_STR from;
    KMXPLUS_STR to;
    KMXPLUS_ELEM mapFrom;
    KMXPLUS_ELEM mapTo;
};

struct COMP_KMXPLUS_TRAN_REORDER {
    KMXPLUS_ELEM elements;
    KMXPLUS_ELEM before;
};

struct COMP_KMXPLUS_TRAN {
  static const KMX_DWORD IDENT = LDML_SECTIONID_TRAN;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD groupCount;
  KMX_DWORD transformCount;
  KMX_DWORD reorderCount;
  // Variable part:
  // COMP_KMXPLUS_TRAN_GROUP groups[]
  // COMP_KMXPLUS_TRAN_TRANSFORM transforms[]
  // COMP_KMXPLUS_TRAN_REORDER reorders[]
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

class COMP_KMXPLUS_TRAN_Helper {
public:
  COMP_KMXPLUS_TRAN_Helper();
  /**
   * Initialize the helper to point at a layr section.
   * @return true if valid
  */
  bool setTran(const COMP_KMXPLUS_TRAN *newTran);
  bool valid() const;

  const COMP_KMXPLUS_TRAN_GROUP     *getGroup(KMX_DWORD n) const;
  const COMP_KMXPLUS_TRAN_TRANSFORM *getTransform(KMX_DWORD n) const;
  const COMP_KMXPLUS_TRAN_REORDER   *getReorder(KMX_DWORD n) const;

private:
  const COMP_KMXPLUS_TRAN *tran;
  bool is_valid;
  const COMP_KMXPLUS_TRAN_GROUP     *groups;
  const COMP_KMXPLUS_TRAN_TRANSFORM *transforms;
  const COMP_KMXPLUS_TRAN_REORDER   *reorders;
};


static_assert(sizeof(struct COMP_KMXPLUS_TRAN) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN) == LDML_LENGTH_TRAN, "mismatched size of section tran");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN_GROUP) == LDML_LENGTH_TRAN_GROUP, "mismatched size of tran group");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN_TRANSFORM) == LDML_LENGTH_TRAN_TRANSFORM, "mismatched size of tran transform");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN_REORDER) == LDML_LENGTH_TRAN_REORDER, "mismatched size of tran reorder");


/* ------------------------------------------------------------------
 * bksp section
   ------------------------------------------------------------------ */

typedef COMP_KMXPLUS_TRAN_Helper COMP_KMXPLUS_BKSP_Helper;

struct COMP_KMXPLUS_BKSP : public COMP_KMXPLUS_TRAN {
  static const KMX_DWORD IDENT = LDML_SECTIONID_BKSP;
};


/* ------------------------------------------------------------------
 * vkey section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_VARS_ITEM {
    KMX_DWORD type;
    KMX_DWORD id;
    KMX_DWORD value;
    KMX_DWORD elem;
};

struct COMP_KMXPLUS_VARS {
  static const KMX_DWORD IDENT = LDML_SECTIONID_VARS;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD markers;
  KMX_DWORD varCount;
  COMP_KMXPLUS_VARS_ITEM varEntries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_VARS) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_VARS) == LDML_LENGTH_VARS, "mismatched size of section vars");
static_assert(sizeof(struct COMP_KMXPLUS_VARS_ITEM) == LDML_LENGTH_VARS_ITEM, "mismatched size of vars item");

/* ------------------------------------------------------------------
 * vkey section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_VKEY_ENTRY {
    KMX_DWORD vkey;
    KMX_DWORD target;
};

struct COMP_KMXPLUS_VKEY {
  static const KMX_DWORD IDENT = LDML_SECTIONID_VKEY;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;
  COMP_KMXPLUS_VKEY_ENTRY entries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_VKEY) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_VKEY) == LDML_LENGTH_VKEY, "mismatched size of section vkey");


/* ------------------------------------------------------------------
 * disp section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_DISP_ENTRY {
    KMX_DWORD to;
    KMX_DWORD display;
};

struct COMP_KMXPLUS_DISP {
  static const KMX_DWORD IDENT = LDML_SECTIONID_DISP;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;
  KMXPLUS_STR baseCharacter;
  COMP_KMXPLUS_DISP_ENTRY entries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_DISP) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_DISP) == LDML_LENGTH_DISP, "mismatched size of section disp");



/* ------------------------------------------------------------------
 * layr section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_LAYR_LIST {
    KMX_DWORD hardware;
    KMX_DWORD layer;
    KMX_DWORD count;
    KMX_DWORD minDeviceWidth;
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_LIST) == LDML_LENGTH_LAYR_LIST, "mismatched size of COMP_KMXPLUS_LAYR_LIST");

struct COMP_KMXPLUS_LAYR_ENTRY {
    KMXPLUS_STR id;
    KMX_DWORD mod;
    KMX_DWORD row;
    KMX_DWORD count;
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_ENTRY) == LDML_LENGTH_LAYR_ENTRY, "mismatched size of COMP_KMXPLUS_LAYR_ENTRY");

struct COMP_KMXPLUS_LAYR_ROW {
    KMX_DWORD key;
    KMX_DWORD count;
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_ROW) == LDML_LENGTH_LAYR_ROW, "mismatched size of COMP_KMXPLUS_LAYR_ROW");


struct COMP_KMXPLUS_LAYR_KEY {
    KMX_DWORD key; // index into key2 section
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_KEY) == LDML_LENGTH_LAYR_KEY, "mismatched size of COMP_KMXPLUS_LAYR_KEY");

struct COMP_KMXPLUS_LAYR {
  static const KMX_DWORD IDENT = LDML_SECTIONID_LAYR;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD listCount;
  KMX_DWORD layerCount;
  KMX_DWORD rowCount;
  KMX_DWORD keyCount;
  // entries, rows, and keys have a dynamic offset
  // use COMP_KMXPLUS_LAYR_Helper to access.
  //
  // COMP_KMXPLUS_LAYR_LIST lists[];
  // COMP_KMXPLUS_LAYR_ENTRY entries[];
  // COMP_KMXPLUS_LAYR_ROW rows[];
  // COMP_KMXPLUS_LAYR_KEY keys[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

/**
 * Helper accessor for the dynamic part of a layr section.
 */
class COMP_KMXPLUS_LAYR_Helper {
public:
  COMP_KMXPLUS_LAYR_Helper();
  /**
   * Initialize the helper to point at a layr section.
   * @return true if valid
  */
  bool setLayr(const COMP_KMXPLUS_LAYR *newLayr);
  bool valid() const;

  const COMP_KMXPLUS_LAYR_LIST  *getList(KMX_DWORD list) const;
  const COMP_KMXPLUS_LAYR_ENTRY *getEntry(KMX_DWORD entry) const;
  const COMP_KMXPLUS_LAYR_ROW   *getRow(KMX_DWORD row) const;
  const COMP_KMXPLUS_LAYR_KEY   *getKey(KMX_DWORD key) const;

private:
  const COMP_KMXPLUS_LAYR *layr;
  bool is_valid;
  const COMP_KMXPLUS_LAYR_LIST *lists;
  const COMP_KMXPLUS_LAYR_ENTRY *entries;
  const COMP_KMXPLUS_LAYR_ROW *rows;
  const COMP_KMXPLUS_LAYR_KEY *keys;
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_LAYR) == LDML_LENGTH_LAYR, "mismatched size of section layr");

/* ------------------------------------------------------------------
 * key2 section
   ------------------------------------------------------------------ */
struct COMP_KMXPLUS_KEYS {
  static const KMX_DWORD IDENT = LDML_SECTIONID_KEYS;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD keyCount;
  KMX_DWORD flicksCount;
  KMX_DWORD flickCount;
  KMX_DWORD kmapCount;
  // see helper for: keys sub-table
  // see helper for: flick lists sub-table
  // see helper for: flick elements sub-table
  // see helper for: kmap sub-table

  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

struct COMP_KMXPLUS_KEYS_FLICK_ELEMENT {
  KMXPLUS_LIST directions;
  KMX_DWORD flags;
  KMXPLUS_STR to; // string or codepoint
  /** get the 'to' string if a char */
  std::u16string get_to_string() const;
};

struct COMP_KMXPLUS_KEYS_FLICK_LIST {
  KMX_DWORD count;
  KMX_DWORD flick; // flick index
  KMXPLUS_STR id;
};

struct COMP_KMXPLUS_KEYS_KEY {
  KMXPLUS_STR to;
  KMX_DWORD flags;
  KMXPLUS_STR id;
  KMXPLUS_STR switchId; // switch
  KMX_DWORD width; // unit: 0.1 keys
  KMXPLUS_LIST longPress;
  KMXPLUS_STR longPressDefault;
  KMXPLUS_LIST multiTap;
  KMX_DWORD flicks; // index

  std::u16string get_to_string() const;
};

struct COMP_KMXPLUS_KEYS_KMAP {
    KMX_DWORD vkey;
    KMX_DWORD mod;
    KMX_DWORD key;     // index into key subtable
};

class COMP_KMXPLUS_KEYS_Helper {
public:
  COMP_KMXPLUS_KEYS_Helper();
  /**
   * Initialize the helper to point at a layr section.
   * @return true if valid
  */
  bool setKeys(const COMP_KMXPLUS_KEYS *newKeys);
  inline bool valid() const { return is_valid; }

  const COMP_KMXPLUS_KEYS_KEY           *getKeys(KMX_DWORD key) const;
  const COMP_KMXPLUS_KEYS_FLICK_LIST    *getFlickLists(KMX_DWORD list) const;
  const COMP_KMXPLUS_KEYS_FLICK_ELEMENT *getFlickElements(KMX_DWORD element) const;
  const COMP_KMXPLUS_KEYS_KMAP          *getKmap(KMX_DWORD element) const;

  /**
   * Search for a key by string id.
   * @param strID id to search for
   * @param index on entry, id to start with such as 0. On exit, index of item if found. Undefined otherwise.
   * @return pointer to key or nullptr
   */
  const COMP_KMXPLUS_KEYS_KEY *findKeyByStringId(KMX_DWORD strId, KMX_DWORD &index) const;
  /**
   * Search for a key by 'to' string id
   * @param str string to search for (for single char strings)
   * @param strID id to search for
   * @param index on entry, id to start with such as 0. On exit, index of item if found. Undefined otherwise.
   * @return pointer to key or nullptr
   */
  const COMP_KMXPLUS_KEYS_KEY *findKeyByStringTo(const std::u16string& str, KMX_DWORD strId, KMX_DWORD &index) const;

private:
  const COMP_KMXPLUS_KEYS *key2;
  bool is_valid;
  const COMP_KMXPLUS_KEYS_KEY *keys;
  const COMP_KMXPLUS_KEYS_FLICK_LIST *flickLists;
  const COMP_KMXPLUS_KEYS_FLICK_ELEMENT *flickElements;
  const COMP_KMXPLUS_KEYS_KMAP *kmap;
};

static_assert(sizeof(struct COMP_KMXPLUS_KEYS_KEY) == LDML_LENGTH_KEYS_KEY, "mismatched size of key2.key");
static_assert(sizeof(struct COMP_KMXPLUS_KEYS_FLICK_ELEMENT) == LDML_LENGTH_KEYS_FLICK_ELEMENT, "mismatched size of key2.flick");
static_assert(sizeof(struct COMP_KMXPLUS_KEYS_FLICK_LIST) == LDML_LENGTH_KEYS_FLICK_LIST, "mismatched size of key2.flicks");
static_assert(sizeof(struct COMP_KMXPLUS_KEYS_KMAP) == LDML_LENGTH_KEYS_KMAP, "mismatched size of key2.kmap");
static_assert(sizeof(struct COMP_KMXPLUS_KEYS) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_KEYS) == LDML_LENGTH_KEYS, "mismatched size of section key2");

/* ------------------------------------------------------------------
 * list section
   ------------------------------------------------------------------ */
struct COMP_KMXPLUS_LIST {
  static const KMX_DWORD IDENT = LDML_SECTIONID_LIST;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD listCount;
  KMX_DWORD indexCount;
  // see helper for: lists sub-table
  // see helper for: indices sub-table

  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

/**
 * list.list subtable
 */
struct COMP_KMXPLUS_LIST_ITEM {
  KMX_DWORD index;
  KMX_DWORD count;
};

/**
 * list.index
 */
typedef KMX_DWORD COMP_KMXPLUS_LIST_INDEX;


class COMP_KMXPLUS_LIST_Helper {
public:
  COMP_KMXPLUS_LIST_Helper();
  /**
   * Initialize the helper to point at a layr section.
   * @return true if valid
  */
  bool setList(const COMP_KMXPLUS_LIST *newList);
  inline bool valid() const { return is_valid; }

  const COMP_KMXPLUS_LIST_ITEM  *getList(KMX_DWORD list) const;
  const COMP_KMXPLUS_LIST_INDEX *getIndex(KMX_DWORD index) const;

private:
  const COMP_KMXPLUS_LIST *list;
  bool is_valid;
  const COMP_KMXPLUS_LIST_ITEM *lists;
  const COMP_KMXPLUS_LIST_INDEX *indices;
};


static_assert(sizeof(struct COMP_KMXPLUS_LIST) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_LIST) == LDML_LENGTH_LIST, "mismatched size of section list");
static_assert(sizeof(struct COMP_KMXPLUS_LIST_ITEM) == LDML_LENGTH_LIST_ITEM, "mismatched size of section list.lists subtable");
static_assert(sizeof(COMP_KMXPLUS_LIST_INDEX) == LDML_LENGTH_LIST_INDEX, "mismatched size of section list.indices subtable");

/**
 * @brief helper accessor object for KMX Plus data
 *
 */
class kmx_plus {
  public:
    /**
     * @brief Construct a new kmx_plus object.
     * Caller must preserve the keyboard file memory
     * until after this object is destroyed.
     *
     * @param keyboard the KMX data
     * @param length length of the entire KMX file
     */
    kmx_plus(const COMP_KEYBOARD *keyboard, size_t length);
    // keep the next elements sorted
    const COMP_KMXPLUS_BKSP *bksp;
    const COMP_KMXPLUS_DISP *disp;
    const COMP_KMXPLUS_ELEM *elem;
    const COMP_KMXPLUS_KEYS *key2;
    const COMP_KMXPLUS_LAYR *layr;
    const COMP_KMXPLUS_LIST *list;
    const COMP_KMXPLUS_LOCA *loca;
    const COMP_KMXPLUS_META *meta;
    const COMP_KMXPLUS_SECT *sect;
    const COMP_KMXPLUS_STRS *strs;
    const COMP_KMXPLUS_TRAN *tran;
    const COMP_KMXPLUS_VARS *vars;
    const COMP_KMXPLUS_VKEY *vkey;
    inline bool is_valid() { return valid; }
    COMP_KMXPLUS_BKSP_Helper bkspHelper;
    COMP_KMXPLUS_KEYS_Helper key2Helper;
    COMP_KMXPLUS_LAYR_Helper layrHelper;
    COMP_KMXPLUS_LIST_Helper listHelper;
    COMP_KMXPLUS_TRAN_Helper tranHelper;
  private:
    bool valid; // true if valid
};

/**
 * See above - undo workaround for variable arrays
 */
#if defined(_WIN32)
#pragma warning ( default : 4200 )
#endif

}  // namespace kmx
}  // namespace kbp
}  // namespace km
