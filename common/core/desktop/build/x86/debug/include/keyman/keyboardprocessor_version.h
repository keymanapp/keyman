#define km_kbp_version_stringify(x) km_kbp_version_to_string(x)
#define km_kbp_version_to_string(x) #x

// Product versioning

#define KM_KBP_VERSION_MAJOR 15
#define KM_KBP_VERSION_MINOR 0
#define KM_KBP_VERSION_PATCH 163
#define KM_KBP_VERSION_STRING km_kbp_version_stringify(15 ## . ## 0 ## . ## 163 ## .0)

// API versioning

#define KM_KBP_LIB_CURRENT  0
#define KM_KBP_LIB_AGE      0
#define KM_KBP_LIB_REVISION 0
