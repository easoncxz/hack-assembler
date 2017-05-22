#!/usr/bin/env ruby

require 'optparse'

require_relative 'update_formula.rb'

def main
  include FormulaManip
  options = parse_opts
  old = Parser::CurrentRuby.parse $stdin.read
  new = put_bottle options[:os], options[:bsum], old
  source = Unparser.unparse new
  $stdout.write source
  $stdout.write "\n"
  nil
end

def parse_opts
  options = {}
  OptionParser.new do |opts|
    opts.banner = 'Put new Bottles into a Homebrew Formula file'
    opts.on(
      '-oOS_VERSION',
      '--os-version=OS_VERSION',
      'Version of Mac OS X, e.g. el_capitan'
    ) do |o|
      options[:os] = o
    end
    opts.on(
      '-cCHECKSUM',
      '--bottle-tar-checksum=CHECKSUM',
      'sha256 checksum of the Bottle tarball'
    ) do |s|
      options[:bsum] = s
    end
  end.parse!
  if !options[:os] || !options[:bsum] then
    puts "Missing arguments. See --help for usage."
    exit 1
  end
  options
end


main if __FILE__ == $0

