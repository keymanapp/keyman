require 'rexml/parsers/pullparser'
require 'fileutils'
module DocBook

  class Epub
    CHECKER = "epubcheck"
    STYLESHEET = File.expand_path(File.join(File.dirname(__FILE__), '..', '..', "docbook.xsl"))
    CALLOUT_PATH = File.join('images', 'callouts')
    CALLOUT_FULL_PATH = File.expand_path(File.join(File.dirname(__FILE__), '..', '..', '..', CALLOUT_PATH))
    CALLOUT_LIMIT = 15
    CALLOUT_EXT = ".png"
    XSLT_PROCESSOR = "xsltproc"
    OUTPUT_DIR = ".epubtmp#{Time.now.to_f.to_s}"
    MIMETYPE = "application/epub+zip"
    META_DIR = "META-INF"
    OEBPS_DIR = "OEBPS"
    ZIPPER = "zip"

    attr_reader :output_dir

    def initialize(docbook_file, output_dir=OUTPUT_DIR)
      @docbook_file = docbook_file
      @output_dir = output_dir
      @meta_dir  = File.join(@output_dir, META_DIR)
      @oebps_dir = File.join(@output_dir, OEBPS_DIR)
      @to_delete = []

      unless File.exist?(@docbook_file)
        raise ArgumentError.new("File #{@docbook_file} does not exist")
      end
    end

    def render_to_file(output_file, verbose=false)
      render_to_epub(output_file, verbose)
      bundle_epub(output_file, verbose)
      cleanup_files(@to_delete)
    end

    def self.invalid?(file)
      # Obnoxiously, we can't just check for a non-zero output...
      cmd = "#{CHECKER} #{file}"
      output = `#{cmd} 2>&1`

      if output == "No errors or warnings detected\n" # TODO wow.. this isn't fragile
        return false
      else  
        STDERR.puts output if $DEBUG
        return output
      end  
    end

    private
    def render_to_epub(output_file, verbose)  
      chunk_quietly = "--stringparam chunk.quietly " + (verbose ? '0' : '1')
      callout_path =  "--stringparam callout.graphics.path #{CALLOUT_PATH}/"
      callout_limit = "--stringparam callout.graphics.number.limit #{CALLOUT_LIMIT}"
      callout_ext =   "--stringparam callout.graphics.extension #{CALLOUT_EXT}" 
      base =          "--stringparam base.dir #{@oebps_dir}/" 
      meta =          "--stringparam epub.metainf.dir #{@meta_dir}/" 
      oebps =         "--stringparam epub.oebps.dir #{@oebps_dir}/" 
      options = "--xinclude #{chunk_quietly} #{callout_path} #{callout_limit} #{callout_ext} #{base} #{meta} #{oebps}"
      # Double-quote stylesheet & file to help Windows cmd.exe
      db2epub_cmd = "#{XSLT_PROCESSOR} #{options} \"#{STYLESHEET}\" \"#{@docbook_file}\""
      STDERR.puts db2epub_cmd if $DEBUG
      success = system(db2epub_cmd)
      raise "Could not render as .epub to #{output_file} (#{db2epub_cmd})" unless success
      @to_delete << Dir["#{@meta_dir}/*"]
      @to_delete << Dir["#{@oebps_dir}/*"]
    end  

    def bundle_epub(output_file, verbose)  
      quiet = verbose ? "" : "-q"
      mimetype_filename = write_mimetype()
      meta  = File.basename(@meta_dir)
      oebps  = File.basename(@oebps_dir)
      images = copy_images()
      callouts = copy_callouts()
      # zip -X -r ../book.epub mimetype META-INF OEBPS
      # Double-quote stylesheet & file to help Windows cmd.exe
      zip_cmd = "cd \"#{@output_dir}\" &&  #{ZIPPER} #{quiet} -X -r  \"#{File.expand_path(output_file)}\" \"#{mimetype_filename}\" \"#{meta}\" \"#{oebps}\""
      puts zip_cmd if $DEBUG
      success = system(zip_cmd)
      raise "Could not bundle into .epub file to #{output_file}" unless success
    end

    def copy_callouts
      new_callout_images = []
      if has_callouts?
        calloutglob = "#{CALLOUT_FULL_PATH}/*#{CALLOUT_EXT}"
        Dir.glob(calloutglob).each {|img|
          img_new_filename = File.join(@oebps_dir, CALLOUT_PATH, File.basename(img))

          # TODO: What to rescue for these two?
          FileUtils.mkdir_p(File.dirname(img_new_filename)) 
          FileUtils.cp(img, img_new_filename)
          @to_delete << img_new_filename
          new_callout_images << img
        }  
      end  
      return new_callout_images
    end

    def copy_images
      image_references = get_image_refs()
      new_images = []
      image_references.each {|img|
        # TODO: It'd be cooler if we had a filetype lookup rather than just
        # extension
        if img =~ /\.(svg|png|gif|jpe?g|xml)/i
          img_new_filename = File.join(@oebps_dir, img)
          img_full = File.join(File.expand_path(File.dirname(@docbook_file)), img)

          # TODO: What to rescue for these two?
          FileUtils.mkdir_p(File.dirname(img_new_filename)) 
          puts(img_full + ": " + img_new_filename) if $DEBUG
          FileUtils.cp(img_full, img_new_filename)
          @to_delete << img_new_filename
          new_images << img_full
        end
      }  
      return new_images
    end

    def write_mimetype
      mimetype_filename = File.join(@output_dir, "mimetype")
      File.open(mimetype_filename, "w") {|f| f.print MIMETYPE}
      @to_delete << mimetype_filename
      return File.basename(mimetype_filename)
    end  

    def cleanup_files(file_list)
      file_list.flatten.each {|f|
        # Yikes
        FileUtils.rm_r(f, :force => true )
      }  
    end  

    # Returns an Array of all of the (image) @filerefs in a document
    def get_image_refs
      parser = REXML::Parsers::PullParser.new(File.new(@docbook_file))
      image_refs = []
      while parser.has_next?
        el = parser.pull
        if el.start_element? and (el[0] == "imagedata" or el[0] == "graphic")
          image_refs << el[1]['fileref'] 
        end  
      end
      return image_refs
    end  

    # Returns true if the document has code callouts
    def has_callouts?
      parser = REXML::Parsers::PullParser.new(File.new(@docbook_file))
      while parser.has_next?
        el = parser.pull
        if el.start_element? and (el[0] == "calloutlist" or el[0] == "co")
          return true
        end  
      end
      return false
    end  
  end
end
