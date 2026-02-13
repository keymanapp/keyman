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
#include <ldml/keyman_core_ldml.h>
#include <list>
#include <deque>

namespace km {
namespace core {
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
typedef KMX_DWORD_unaligned KMXPLUS_STR;
/**
 * Indicates an offset into the list table (0 = zero length)
*/
typedef KMX_DWORD_unaligned KMXPLUS_LIST;
/**
 * Indicates an offset into the elem table (0 = zero length)
*/
typedef KMX_DWORD_unaligned KMXPLUS_ELEM;
/**
 * Indicates a 4-byte identity
*/
typedef KMX_DWORD_unaligned KMXPLUS_IDENT;

// forward declarations
struct COMP_KMXPLUS_TRAN;
struct COMP_KMXPLUS_TRAN_GROUP;
struct COMP_KMXPLUS_TRAN_TRANSFORM;
struct COMP_KMXPLUS_TRAN_REORDER;
struct COMP_KMXPLUS_STRS;
class COMP_KMXPLUS_STRS_Helper;

/**
 * @brief Binary-backed version of section header, v17, to be used internally
 * only when loading; copied into COMP_KMXPLUS_HEADER for use
 */
struct COMP_KMXPLUS_HEADER_17 {
  KMXPLUS_IDENT ident;       // 0000 Section name
  KMX_DWORD_unaligned size;  // 0004 Section length
  bool valid(KMX_DWORD length) const;
};

/**
 * @brief Binary-backed version of section header, v19, to be used internally
 * only when loading; copied into COMP_KMXPLUS_HEADER for use
 */
struct COMP_KMXPLUS_HEADER_19 {
  KMXPLUS_IDENT ident;       // 0000 Section name
  KMX_DWORD_unaligned size;  // 0004 Section length
  KMX_DWORD_unaligned version; // 0008 Section version
  bool valid(KMX_DWORD length) const;
};

/**
 * @brief In-memory version of section header, copied from binary data e.g.
 * COMP_KMXPLUS_HEADER_19, and including file version and other useful metadata
 */
struct COMP_KMXPLUS_HEADER {
private:
  KMX_DWORD _fileVersion;
  KMX_DWORD _headerSize;
public:
  // TODO: read only
  KMXPLUS_IDENT ident;
  KMX_DWORD size;
  KMX_DWORD version;

  inline KMX_DWORD fileVersion() const {
    return _fileVersion;
  }

  inline KMX_DWORD headerSize() const {
    return _headerSize;
  }

  /**
   * @brief Calculate element size, taking into account change in header size
   * from v17 to v19
   *
   * @param size
   * @return KMX_DWORD
   */
  inline KMX_DWORD calculateBaseSize(KMX_DWORD elementSize) {
    return elementSize - LDML_LENGTH_HEADER_17 + this->_headerSize;
  }

  inline void set(KMX_DWORD fileVersion, KMXPLUS_IDENT identIn, KMX_DWORD sizeIn, KMX_DWORD versionIn = LDML_KMXPLUS_VERSION_17) {
    this->_fileVersion = fileVersion;
    this->_headerSize = fileVersion == LDML_KMXPLUS_VERSION_17 ? LDML_LENGTH_HEADER_17 : LDML_LENGTH_HEADER_19;
    this->ident = identIn;
    this->size = sizeIn;
    this->version = versionIn;
  }
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_HEADER_17) == LDML_LENGTH_HEADER_17, "mismatched size of section header");
// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_HEADER_19) == LDML_LENGTH_HEADER_19, "mismatched size of section header");

/* ------------------------------------------------------------------
 * Section helper base class - all sections have a helper class
   ------------------------------------------------------------------ */

template<class T>
class COMP_KMXPLUS_Section_Helper {
private:
  const T* _data;

protected:
  const T* data() const {
    return _data;
  }

public:
  COMP_KMXPLUS_HEADER header;
  COMP_KMXPLUS_Section_Helper() : _data(nullptr) {}
  virtual ~COMP_KMXPLUS_Section_Helper() {}
  virtual bool set(const T* section) {
    _data = section;
    return true;
  }
};

/* ------------------------------------------------------------------
 * sect section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_SECT_ENTRY {
  KMXPLUS_IDENT sect;          // 0010+ Section identity
  KMX_DWORD_unaligned offset;  // 0014+ Section offset relative to dpKMXPlus of section
};

struct COMP_KMXPLUS_SECT {
  // static_assert(std::is_base_of<COMP_KMXPLUS_HEADER, header>::value, "header must be a descendant of COMP_KMXPLUS_HEADER");
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_SECT;
  static const KMXPLUS_IDENT IDENT_V19 = LDML_SECTIONID_SEC2;
  KMX_DWORD_unaligned total;          // 0008 KMXPlus entire length
  KMX_DWORD_unaligned count;          // 000C number of section headers
  COMP_KMXPLUS_SECT_ENTRY entries[];  // 0010 section entries
  /**
   * @brief Get the offset of a section
   *
   * @param ident section id such as 'strs'. Never 'sect' (the sect table does not list itself!)
   * @return KMX_DWORD offset from beginning of kmxplus, or 0 if not found
   */
  KMX_DWORD find(KMX_DWORD ident) const;
  /**
   * @brief Get the pointer to a specific section
   *
   * @param header       reference to the 'sect' section header for the file
   * @param ident        section id such as 'strs'. Never 'sect' (the sect table does not list itself!)
   * @param entryLength  on exit, will be set to the possible length of the section (based on the remainder of the KMX+ file)
   * @return pointer to raw bytes of requested section, or nullptr if not found
   */
  const uint8_t *get(COMP_KMXPLUS_HEADER const& header, KMX_DWORD ident, KMX_DWORD &entryLength) const;
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'sect' section header for the file
   * @param fileLength   length of the KMX+ file data (not including KMX data, if KMX+ is embedded)
   * Does not validate the entire file.
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD fileLength) const;
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_SECT) == LDML_LENGTH_SECT - LDML_LENGTH_HEADER_17, "mismatched size of section sect");
static_assert(sizeof(struct COMP_KMXPLUS_SECT) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");

class COMP_KMXPLUS_SECT_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_SECT> {};

/* ------------------------------------------------------------------
 * elem section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_ELEM_ELEMENT {
  KMX_DWORD_unaligned element;  // str: output string or UTF-32LE codepoint
  KMX_DWORD_unaligned flags;    // flag and order values
  /**
   * @brief Get the 'element' as a string, if flags&LDML_ELEM_FLAGS_TYPE = CHAR
   *
   * @return std::u16string
   */
  std::u16string get_element_string() const;

  /**
   * @brief load this[0]…this[length] as a string list
   * @param length number of elements, including this one
   * @return the string elements as a string array
  */
  std::deque<std::u32string> loadAsStringList(KMX_DWORD length, const km::core::kmx::COMP_KMXPLUS_STRS_Helper &strs) const;

  /** @return element type */
  KMX_DWORD type() const;
};

struct COMP_KMXPLUS_ELEM_ENTRY {
  KMX_DWORD_unaligned offset;  // 0010+ offset from this blob
  KMX_DWORD_unaligned length;  // 0014+ str length (ELEMENT units)
};

struct COMP_KMXPLUS_ELEM {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_ELEM;
  KMX_DWORD_unaligned count;                    // 0008 count of str entries
  COMP_KMXPLUS_ELEM_ENTRY entries[];  // 000C+ entries

  /**
   * @brief True if section is valid.
   * @param header       reference to the 'elem' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;

  /**
   * @brief
   * @param header        reference to the 'elem' section header
   * @param elementNumber element number, 0..count-1
   * @param length        fillin: length of list
   * @return pointer to first element of list of length length. or nullptr
   */
  const COMP_KMXPLUS_ELEM_ELEMENT *getElementList(
    COMP_KMXPLUS_HEADER const &header,
    KMX_DWORD elementNumber,
    KMX_DWORD &length
  ) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_ELEM) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_ELEM) == LDML_LENGTH_ELEM - LDML_LENGTH_HEADER_17, "mismatched size of section elem");

class COMP_KMXPLUS_ELEM_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_ELEM> {
public:
  /**
   * @param elementNumber element number, 0..count-1
   * @param length fillin: length of list
   * @return pointer to first element of list of length length. or nullptr
   */
  const COMP_KMXPLUS_ELEM_ELEMENT *getElementList(
    KMX_DWORD elementNumber,
    KMX_DWORD &length
  ) const;
};

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
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_LOCA;
  KMX_DWORD_unaligned count; // 0008 number of locales
  COMP_KMXPLUS_LOCA_ENTRY entries[];
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'loca' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_LOCA) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_LOCA) == LDML_LENGTH_LOCA - LDML_LENGTH_HEADER_17, "mismatched size of section loca");

class COMP_KMXPLUS_LOCA_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_LOCA> {};

/* ------------------------------------------------------------------
 * meta section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_META {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_META;
  KMXPLUS_STR author;
  KMXPLUS_STR conform;
  KMXPLUS_STR layout;
  KMXPLUS_STR name;
  KMXPLUS_STR indicator;
  KMXPLUS_STR version;
  KMX_DWORD_unaligned settings;
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'meta' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;

  /** @brief True if normalization disabled*/
  bool normalization_disabled() const {
    return settings & LDML_META_SETTINGS_NORMALIZATION_DISABLED;
  }
};

static_assert(sizeof(struct COMP_KMXPLUS_META) == LDML_LENGTH_META - LDML_LENGTH_HEADER_17, "mismatched size of section meta");

class COMP_KMXPLUS_META_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_META> {};

/* ------------------------------------------------------------------
 * strs section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_STRS_ENTRY {
    KMX_DWORD_unaligned offset;                 // 0010+ offset from this blob
    KMX_DWORD_unaligned length;                 // 0014+ str length (UTF-16LE units)
};

struct COMP_KMXPLUS_STRS {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_STRS;
  KMX_DWORD_unaligned count;                    // 0008 count of str entries
  COMP_KMXPLUS_STRS_ENTRY entries[];  // 0010+ entries

  /**
   * @brief Get a string entry
   *
   * @param header        reference to the 'strs' section header
   * @param entry         entry number
   * @return nullptr or a pointer to the output buffer
   */
  std::u16string get(const COMP_KMXPLUS_HEADER& header, KMX_DWORD entry) const;

  /**
   * Slow search
   * @param header        reference to the 'strs' section header
   */
  KMX_DWORD find(const COMP_KMXPLUS_HEADER& header, const std::u16string&) const;
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'strs' section header
   * @param length       length of the section in bytes
   */
  bool valid(const COMP_KMXPLUS_HEADER& header, KMX_DWORD length) const;

  /** convert a single char to a string*/
  static std::u16string str_from_char(KMX_DWORD v);

  static bool valid_string(const KMX_WCHAR *start, KMX_DWORD length);
};

static_assert(sizeof(struct COMP_KMXPLUS_STRS) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_STRS) == LDML_LENGTH_STRS - LDML_LENGTH_HEADER_17, "mismatched size of section strs");

class COMP_KMXPLUS_STRS_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_STRS> {
public:
  std::u16string get(KMX_DWORD entry) const;
  KMX_DWORD find(const std::u16string&) const;
};

/* ------------------------------------------------------------------
 * tran section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_TRAN_GROUP {
    KMX_DWORD_unaligned type;
    KMX_DWORD_unaligned count;
    KMX_DWORD_unaligned index;
};

struct COMP_KMXPLUS_TRAN_TRANSFORM {
    KMXPLUS_STR from;
    KMXPLUS_STR to;
    KMXPLUS_STR mapFrom; // variable name
    KMXPLUS_STR mapTo;   // variable name
};

struct COMP_KMXPLUS_TRAN_REORDER {
    KMXPLUS_ELEM elements;
    KMXPLUS_ELEM before;
};

struct COMP_KMXPLUS_TRAN {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_TRAN;
  KMX_DWORD_unaligned groupCount;
  KMX_DWORD_unaligned transformCount;
  KMX_DWORD_unaligned reorderCount;
  // Variable part:
  // COMP_KMXPLUS_TRAN_GROUP groups[]
  // COMP_KMXPLUS_TRAN_TRANSFORM transforms[]
  // COMP_KMXPLUS_TRAN_REORDER reorders[]
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'tran' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
};

class COMP_KMXPLUS_TRAN_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_TRAN> {
public:
  COMP_KMXPLUS_TRAN_Helper();
  /**
   * Initialize the helper to point at a tran or bksp section.
   * @return true if valid
  */
  virtual bool set(const COMP_KMXPLUS_TRAN *section);
  virtual bool valid() const;

  const COMP_KMXPLUS_TRAN_GROUP     *getGroup(KMX_DWORD n) const;
  const COMP_KMXPLUS_TRAN_TRANSFORM *getTransform(KMX_DWORD n) const;
  const COMP_KMXPLUS_TRAN_REORDER   *getReorder(KMX_DWORD n) const;

private:
  bool is_valid;
  const COMP_KMXPLUS_TRAN_GROUP     *groups;
  const COMP_KMXPLUS_TRAN_TRANSFORM *transforms;
  const COMP_KMXPLUS_TRAN_REORDER   *reorders;
};

static_assert(sizeof(struct COMP_KMXPLUS_TRAN) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN) == LDML_LENGTH_TRAN - LDML_LENGTH_HEADER_17, "mismatched size of section tran");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN_GROUP) == LDML_LENGTH_TRAN_GROUP, "mismatched size of tran group");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN_TRANSFORM) == LDML_LENGTH_TRAN_TRANSFORM, "mismatched size of tran transform");
static_assert(sizeof(struct COMP_KMXPLUS_TRAN_REORDER) == LDML_LENGTH_TRAN_REORDER, "mismatched size of tran reorder");

// assert some parallel constants
static_assert(LDML_UC_SENTINEL == UC_SENTINEL, "mismatch: LDML_UC_SENTINEL");
static_assert(LDML_MARKER_CODE == CODE_DEADKEY, "mismatch: LDML_MARKER_CODE");
static_assert(LDML_MARKER_ANY_INDEX < UC_SENTINEL, "expected LDML_MARKER_ANY_INDEX < UC_SENTINEL");

/** @returns true if a valid marker per spec */
static inline bool is_valid_marker(KMX_DWORD marker_no) {
  return ((marker_no == LDML_MARKER_ANY_INDEX) || (marker_no >= LDML_MARKER_MIN_INDEX && marker_no <= LDML_MARKER_MAX_INDEX));
}

/* ------------------------------------------------------------------
 * bksp section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_BKSP : public COMP_KMXPLUS_TRAN {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_BKSP;
};

typedef COMP_KMXPLUS_TRAN_Helper COMP_KMXPLUS_BKSP_Helper;

/* ------------------------------------------------------------------
 * vars section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_VARS_ITEM {
    KMX_DWORD_unaligned type;
    KMXPLUS_STR id;
    KMXPLUS_STR value;
    KMXPLUS_ELEM elem;
};

struct COMP_KMXPLUS_VARS {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_VARS;
  KMXPLUS_LIST markers;
  KMX_DWORD_unaligned varCount;
  COMP_KMXPLUS_VARS_ITEM varEntries[];
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'vars' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;

  const COMP_KMXPLUS_VARS_ITEM *findByStringId(KMX_DWORD strId) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_VARS) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_VARS) == LDML_LENGTH_VARS - LDML_LENGTH_HEADER_17, "mismatched size of section vars");
static_assert(sizeof(struct COMP_KMXPLUS_VARS_ITEM) == LDML_LENGTH_VARS_ITEM, "mismatched size of vars item");

class COMP_KMXPLUS_VARS_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_VARS> {};

/* ------------------------------------------------------------------
 * disp section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_DISP_ENTRY_17 {
    KMXPLUS_STR to;
    KMXPLUS_STR id;
    KMXPLUS_STR display;
};

struct COMP_KMXPLUS_DISP_ENTRY_19 {
    KMXPLUS_STR toId;
    KMXPLUS_STR display;
    KMX_DWORD_unaligned flags;
};

struct COMP_KMXPLUS_DISP {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_DISP;
  KMX_DWORD_unaligned count;
  KMXPLUS_STR baseCharacter;
  COMP_KMXPLUS_DISP_ENTRY_19 entries[];
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'disp' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
  bool valid_19(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
  bool valid_17(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_DISP) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_DISP) == LDML_LENGTH_DISP - LDML_LENGTH_HEADER_17, "mismatched size of section disp");

class COMP_KMXPLUS_DISP_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_DISP> {
  virtual bool set(const COMP_KMXPLUS_DISP *newDisp);
};

/* ------------------------------------------------------------------
 * layr section
   ------------------------------------------------------------------ */

struct COMP_KMXPLUS_LAYR_FORM_V17 {
    KMX_DWORD_unaligned hardware;
    KMX_DWORD_unaligned layer;
    KMX_DWORD_unaligned count;
    KMX_DWORD_unaligned minDeviceWidth;
};

struct COMP_KMXPLUS_LAYR_FORM_V19 {
    KMX_DWORD_unaligned hardware;
    KMX_DWORD_unaligned layer;
    KMX_DWORD_unaligned count;
    KMX_DWORD_unaligned minDeviceWidth;
    KMXPLUS_STR         baseLayout;        // v19: str: identifier for base layout (reserved)
    KMXPLUS_STR         fontFaceName;      // v19: str: font face name
    KMX_DWORD_unaligned fontSizePct;       // v19: font size in % of default size
    KMX_DWORD_unaligned flags;             // v19: flags
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_FORM_V17) == LDML_LENGTH_LAYR_FORM_V17, "mismatched size of COMP_KMXPLUS_LAYR_FORM_V17");
static_assert(sizeof(struct COMP_KMXPLUS_LAYR_FORM_V19) == LDML_LENGTH_LAYR_FORM_V19, "mismatched size of COMP_KMXPLUS_LAYR_FORM_V19");

struct COMP_KMXPLUS_LAYR_ENTRY {
    KMXPLUS_STR id;
    KMX_DWORD_unaligned mod;
    KMX_DWORD_unaligned row;
    KMX_DWORD_unaligned count;
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_ENTRY) == LDML_LENGTH_LAYR_ENTRY, "mismatched size of COMP_KMXPLUS_LAYR_ENTRY");

struct COMP_KMXPLUS_LAYR_ROW {
    KMX_DWORD_unaligned key;
    KMX_DWORD_unaligned count;
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_ROW) == LDML_LENGTH_LAYR_ROW, "mismatched size of COMP_KMXPLUS_LAYR_ROW");


struct COMP_KMXPLUS_LAYR_KEY {
    KMX_DWORD_unaligned key; // index into key2 section
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR_KEY) == LDML_LENGTH_LAYR_KEY, "mismatched size of COMP_KMXPLUS_LAYR_KEY");

struct COMP_KMXPLUS_LAYR {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_LAYR;
  KMX_DWORD_unaligned formCount;
  KMX_DWORD_unaligned layerCount;
  KMX_DWORD_unaligned rowCount;
  KMX_DWORD_unaligned keyCount;
  // entries, rows, and keys have a dynamic offset
  // use COMP_KMXPLUS_LAYR_Helper to access.
  //
  // COMP_KMXPLUS_LAYR_FORM forms[];
  // COMP_KMXPLUS_LAYR_ENTRY entries[];
  // COMP_KMXPLUS_LAYR_ROW rows[];
  // COMP_KMXPLUS_LAYR_KEY keys[];
  /**
   * @brief True if section is valid.
   * @param header       reference to the 'layr' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
  bool valid_19(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
  bool valid_17(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
};

/**
 * Helper accessor for the dynamic part of a layr section.
 */
class COMP_KMXPLUS_LAYR_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_LAYR> {
public:
  COMP_KMXPLUS_LAYR_Helper();
  ~COMP_KMXPLUS_LAYR_Helper() {
    if(own_forms) {
      delete [] forms;
      forms = nullptr;
    }
  }
  /**
   * Initialize the helper to point at a layr section.
   * @return true if valid
  */
  virtual bool set(const COMP_KMXPLUS_LAYR *newLayr);
  virtual bool valid() const;

  /**
   * @param form index from 0 to layr->formCount
   */
  const COMP_KMXPLUS_LAYR_FORM_V19  *getForm(KMX_DWORD form) const;
  /**
   * @param entry index value: COMP_KMXPLUS_LAYR_FORM.layer but less than COMP_KMXPLUS_LAYR_FORM.layer+COMP_KMXPLUS_LAYR_FORM.count
   */
  const COMP_KMXPLUS_LAYR_ENTRY *getEntry(KMX_DWORD entry) const;
  const COMP_KMXPLUS_LAYR_ROW   *getRow(KMX_DWORD row) const;
  const COMP_KMXPLUS_LAYR_KEY   *getKey(KMX_DWORD key) const;

private:
  bool is_valid;
  bool own_forms;
  const COMP_KMXPLUS_LAYR_FORM_V19 *forms;
  const COMP_KMXPLUS_LAYR_ENTRY *entries;
  const COMP_KMXPLUS_LAYR_ROW *rows;
  const COMP_KMXPLUS_LAYR_KEY *keys;
};

static_assert(sizeof(struct COMP_KMXPLUS_LAYR) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_LAYR) == LDML_LENGTH_LAYR - LDML_LENGTH_HEADER_17, "mismatched size of section layr");

/* ------------------------------------------------------------------
 * key2 section
   ------------------------------------------------------------------ */
struct COMP_KMXPLUS_KEYS {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_KEYS;
  KMX_DWORD_unaligned keyCount;
  KMX_DWORD_unaligned flicksCount;
  KMX_DWORD_unaligned flickCount;
  KMX_DWORD_unaligned kmapCount;
  // see helper for: keys sub-table
  // see helper for: flick lists sub-table
  // see helper for: flick elements sub-table
  // see helper for: kmap sub-table

  /**
   * @brief True if section is valid.
   * @param header       reference to the 'keys' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
};

struct COMP_KMXPLUS_KEYS_FLICK_ELEMENT {
  KMXPLUS_LIST directions;
  KMXPLUS_STR to; // string
  std::u16string get_to_string() const;
};

struct COMP_KMXPLUS_KEYS_FLICK_LIST {
  KMX_DWORD_unaligned count;
  KMX_DWORD_unaligned flick; // flick index
  KMXPLUS_STR id;
};

struct COMP_KMXPLUS_KEYS_KEY {
  KMXPLUS_STR to;
  KMX_DWORD_unaligned flags;
  KMXPLUS_STR id;
  KMXPLUS_STR switchId; // switch
  KMX_DWORD_unaligned width; // unit: 0.1 keys
  KMXPLUS_LIST longPress;
  KMXPLUS_STR longPressDefault;
  KMXPLUS_LIST multiTap;
  KMX_DWORD_unaligned flicks; // index

  std::u16string get_to_string() const;
};

struct COMP_KMXPLUS_KEYS_KMAP {
    KMX_DWORD_unaligned vkey;
    KMX_DWORD_unaligned mod;
    KMX_DWORD_unaligned key;     // index into key subtable
};

class COMP_KMXPLUS_KEYS_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_KEYS> {
public:
  COMP_KMXPLUS_KEYS_Helper();
  /**
   * Initialize the helper to point at a layr section.
   * @return true if valid
  */
  virtual bool set(const COMP_KMXPLUS_KEYS *newKeys);
  virtual bool valid() const { return is_valid; }

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
static_assert(sizeof(struct COMP_KMXPLUS_KEYS) == LDML_LENGTH_KEYS - LDML_LENGTH_HEADER_17, "mismatched size of section key2");

/* ------------------------------------------------------------------
 * list section
   ------------------------------------------------------------------ */
struct COMP_KMXPLUS_LIST {
  static const KMXPLUS_IDENT IDENT = LDML_SECTIONID_LIST;
  KMX_DWORD_unaligned listCount;
  KMX_DWORD_unaligned indexCount;
  // see helper for: lists sub-table
  // see helper for: indices sub-table

  /**
   * @brief True if section is valid.
   * @param header       reference to the 'list' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
};

/**
 * list.list subtable
 */
struct COMP_KMXPLUS_LIST_ITEM {
  KMX_DWORD_unaligned index;
  KMX_DWORD_unaligned count;
};

/**
 * list.index
 */
typedef KMX_DWORD_unaligned COMP_KMXPLUS_LIST_INDEX;


class COMP_KMXPLUS_LIST_Helper : public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_LIST> {
public:
  COMP_KMXPLUS_LIST_Helper();
  /**
   * Initialize the helper to point at a layr section.
   * @return true if valid
  */
  virtual bool set(const COMP_KMXPLUS_LIST *newList);
  virtual bool valid() const { return is_valid; }

  const COMP_KMXPLUS_LIST_ITEM  *getList(KMX_DWORD list) const;
  const COMP_KMXPLUS_LIST_INDEX *getIndex(KMX_DWORD index) const;

private:
  bool is_valid;
  const COMP_KMXPLUS_LIST_ITEM *lists;
  const COMP_KMXPLUS_LIST_INDEX *indices;
};


static_assert(sizeof(struct COMP_KMXPLUS_LIST) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_LIST) == LDML_LENGTH_LIST - LDML_LENGTH_HEADER_17, "mismatched size of section list");
static_assert(sizeof(struct COMP_KMXPLUS_LIST_ITEM) == LDML_LENGTH_LIST_ITEM, "mismatched size of section list.lists subtable");
static_assert(sizeof(COMP_KMXPLUS_LIST_INDEX) == LDML_LENGTH_LIST_INDEX, "mismatched size of section list.indices subtable");



/* ------------------------------------------------------------------
 * uset section
   ------------------------------------------------------------------ */
/**
 * uset.range index
 */
typedef KMX_DWORD KMXPLUS_USET;

struct COMP_KMXPLUS_USET {
  static const KMX_DWORD IDENT = LDML_SECTIONID_USET;
  KMX_DWORD_unaligned usetCount;
  KMX_DWORD_unaligned rangeCount;
  // see helper for: usets sub-table
  // see helper for: ranges sub-table

  /**
   * @brief True if section is valid.
   * @param header       reference to the 'uset' section header
   * @param length       length of the section in bytes
   */
  bool valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const;
};

/**
 * uset.usets subtable
 */
struct COMP_KMXPLUS_USET_USET {
  KMX_DWORD range;
  KMX_DWORD count;
  KMXPLUS_STR pattern;
};

struct COMP_KMXPLUS_USET_RANGE {
  km_core_usv start;
  km_core_usv end;
  public:
    COMP_KMXPLUS_USET_RANGE(const COMP_KMXPLUS_USET_RANGE& other);
    COMP_KMXPLUS_USET_RANGE(KMX_DWORD start, KMX_DWORD end);
};

/**
 * represents one of the uset elements.
 * TODO-LDML: replace this with a real icu::UnicodeSet? or at least
 * a function producing the same?
 */
class SimpleUSet {
  public:
    /** construct a set over the specified range. Data is copied. */
    SimpleUSet(const COMP_KMXPLUS_USET_RANGE* newStart, size_t newCount);
    /** empty set */
    SimpleUSet();
    /** true if the uset contains this char */
    bool contains(km_core_usv ch) const;
    /** debugging */
    void dump() const;
    bool valid() const;
  private:
    std::list<COMP_KMXPLUS_USET_RANGE> ranges;
};

class COMP_KMXPLUS_USET_Helper: public COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_USET> {
public:
  COMP_KMXPLUS_USET_Helper();
  /**
   * Initialize the helper to point at a uset section.
   * @return true if valid
  */
  virtual bool set(const COMP_KMXPLUS_USET *newUset);
  virtual bool valid() const { return is_valid; }

  SimpleUSet getUset(KMXPLUS_USET list) const;
  const COMP_KMXPLUS_USET_RANGE *getRange(KMX_DWORD index) const;

private:
  bool is_valid;
  const COMP_KMXPLUS_USET_USET *usets;
  const COMP_KMXPLUS_USET_RANGE *ranges;
};

static_assert(sizeof(struct COMP_KMXPLUS_USET) % 0x4 == 0, "Structs prior to variable part should align to 32-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_USET) == LDML_LENGTH_USET - LDML_LENGTH_HEADER_17, "mismatched size of section uset");
static_assert(sizeof(struct COMP_KMXPLUS_USET_RANGE) == LDML_LENGTH_USET_RANGE, "mismatched size of section uset.ranges subtable");
static_assert(sizeof(struct COMP_KMXPLUS_USET_USET) == LDML_LENGTH_USET_USET, "mismatched size of section uset.usets subtable");

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
    // TODO: put data inside the helper only, rename
    const COMP_KMXPLUS_BKSP *bksp; COMP_KMXPLUS_TRAN_Helper bkspHelper;
    const COMP_KMXPLUS_DISP *disp; COMP_KMXPLUS_DISP_Helper dispHelper;
    const COMP_KMXPLUS_ELEM *elem; COMP_KMXPLUS_ELEM_Helper elemHelper;
    const COMP_KMXPLUS_KEYS *key2; COMP_KMXPLUS_KEYS_Helper key2Helper;
    const COMP_KMXPLUS_LAYR *layr; COMP_KMXPLUS_LAYR_Helper layrHelper;
    const COMP_KMXPLUS_LIST *list; COMP_KMXPLUS_LIST_Helper listHelper;
    const COMP_KMXPLUS_LOCA *loca; COMP_KMXPLUS_LOCA_Helper locaHelper;
    const COMP_KMXPLUS_META *meta; COMP_KMXPLUS_META_Helper metaHelper;
    const COMP_KMXPLUS_SECT *sect; COMP_KMXPLUS_SECT_Helper sectHelper;
    const COMP_KMXPLUS_STRS *strs; COMP_KMXPLUS_STRS_Helper strsHelper;
    const COMP_KMXPLUS_TRAN *tran; COMP_KMXPLUS_TRAN_Helper tranHelper;
    const COMP_KMXPLUS_USET *uset; COMP_KMXPLUS_USET_Helper usetHelper;
    const COMP_KMXPLUS_VARS *vars; COMP_KMXPLUS_VARS_Helper varsHelper;
    inline bool is_valid() { return valid; }
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
}  // namespace core
}  // namespace km
