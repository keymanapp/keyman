/*    
    file
        main.c

    function
        to manipulate text input source, i.e., 
            - print currently selected source,
            - select specified source (enable it as needed)
            - enable specified source, 
            - disable specified source,
            - toggle enabled/disabled state of specified source.

    compile
        gcc -framework Carbon -o textinputsource main.c

    usage e.g.
        ./textinputsource [-s|e|d|t name]

        given no arguments, it will print the current source name.
        options:
            -s : select source name (enable it as needed)
            -e : enable source name
            -d : disable source name
            -t : toggle enabled/disabled on source name


    Originally found at https://discussions.apple.com/thread/5610262
*/

#include <Carbon/Carbon.h>
#include <libgen.h>    // basename

TISInputSourceRef getInputSourceByName(char *);

int
main (int argc, char * argv[])
{
    int ret = -1;
    int c;
    TISInputSourceRef tis;
    CFStringRef name;
    OSStatus err = 0;

    while ((c = getopt(argc, argv, "s:e:d:t:h")) != -1)
    {
        switch (c)
        {
            case 's':
                tis = getInputSourceByName(optarg);
                if (tis)
                {
                    CFBooleanRef enabled = TISGetInputSourceProperty(tis, kTISPropertyInputSourceIsEnabled);
                    if (enabled == kCFBooleanFalse)
                        TISEnableInputSource(tis);
                    err = TISSelectInputSource(tis);
                    CFRelease(tis);
                }
                ret = tis ? (int) err : 1;
                break;
            case 'e':
                tis = getInputSourceByName(optarg);
                if (tis)
                {
                    err = TISEnableInputSource(tis);
                    CFRelease(tis);
                }
                ret = tis ? (int) err : 1;
                break;
            case 'd':
                tis = getInputSourceByName(optarg);
                if (tis)
                {
                    err = TISDisableInputSource(tis);
                    CFRelease(tis);
                }
                ret = tis ? (int) err : 1;
                break;
            case 't':
                tis = getInputSourceByName(optarg);
                if (tis)
                {
                    CFBooleanRef enabled = TISGetInputSourceProperty(tis, kTISPropertyInputSourceIsEnabled);
                    if (enabled == kCFBooleanTrue)
                        err = TISDisableInputSource(tis);
                    else
                        err = TISEnableInputSource(tis);
                    CFRelease(tis);
                }
                ret = tis ? (int) err : 1;
                break;
            case 'h':
            case '?':
            default:
                fprintf(stderr, "Usage: %s %s\n\t%s\n\t%s\n\t%s\n\t%s\n\t%s\n",
                    basename(argv[0]),
                    "[-s|e|d|t name]",
                    "-s : select source name (enable it as needed)",
                    "-e : enable source name",
                    "-d : disable source name",
                    "-t : toggle enabled/disabled of source name",
                    "no arguments : print current source name"
                );
                ret = 1;
                break;
        }
    }
    if (ret == -1) // no args: print current keyboard input source
    {
        tis = TISCopyCurrentKeyboardInputSource();
        name = TISGetInputSourceProperty(tis, kTISPropertyLocalizedName);
        int len = CFStringGetLength(name) * 4 + 1;
        char cname[len];
        bool b = CFStringGetCString(name, cname, len, kCFStringEncodingUTF8);
        printf("%s\n", b ? cname : "");
        ret = b ? 0 : 1;
    }
    if (err != noErr)
        fprintf(stderr, "Text Input Source Services error: %d\n", (int) err);
    return ret;
}

TISInputSourceRef
getInputSourceByName(char *cname)
{
    //     char *cname : input source name in UTF-8 terminated by null character
    //     return TISInputSourceRef or NULL : text input source reference (retained)

    CFStringRef name = CFStringCreateWithCString(kCFAllocatorDefault, cname, kCFStringEncodingUTF8);
    CFStringRef keys[] = { kTISPropertyLocalizedName };
    CFStringRef values[] = { name };
    CFDictionaryRef dict = CFDictionaryCreate(kCFAllocatorDefault, (const void **)keys, (const void **)values, 1, NULL, NULL);
    CFArrayRef array = TISCreateInputSourceList(dict, true);
    CFRelease(dict);
    CFRelease(name);
    if (!array)
    {
        fprintf(stderr, "No such text input source: %s\n", optarg);
        return NULL;
    }
    TISInputSourceRef tis = (TISInputSourceRef) CFArrayGetValueAtIndex(array, 0);
    CFRetain(tis);
    CFRelease(array);
    return tis;
}