#!/usr/bin/env ruby

require 'optparse'

require_relative 'update_formula.rb'

def main
  include FormulaManip
  options = parse_opts
  ast = Parser::Ruby21.parse $stdin.read
  ast = update_formula_field "url", options[:surl], ast
  ast = update_formula_field "sha256", options[:ssum], ast
  source = Unparser.unparse ast
  $stdout.write source
  $stdout.write "\n"
  nil
end

def parse_opts
  options = {}
  OptionParser.new do |opts|
    opts.banner = 'Modify a Homebrew Formula file'
    opts.on(
      '-sURL',
      '--source-tar-url=URL',
      'URL to the source tarball for compiled installation'
    ) do |u|
      options[:surl] = u
    end
    opts.on(
      '-cCHECKSUM',
      '--source-tar-checksum=CHECKSUM',
      'sha256 checksum of the source tarball'
    ) do |s|
      options[:ssum] = s
    end
  end.parse!
  if !options[:surl] || !options[:ssum] then
    puts "Missing arguments. See --help for usage."
    exit 1
  end
  options
end


main if __FILE__ == $0

