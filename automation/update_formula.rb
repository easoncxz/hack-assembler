#!/usr/bin/env ruby

require 'optparse'
require 'parser/current'
require 'unparser'

Parser::Builders::Default.emit_lambda = true
Parser::Builders::Default.emit_procarg0 = true

def main
  options = parse_opts
  ast = Parser::CurrentRuby.parse $stdin.read
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

# update_formula_field :: String -> String -> Node -> Node
def update_formula_field field, value, klass
  update(
    klass,
    [ by_type('begin'),
      by_both(
        by_type('send'),
        by_msg(field)),
      by_type('str')],
    -> (n) { n.updated(nil, [value]) })
end

# type Choice = Proc (Node -> Bool)
# update :: Node -> [Choice] -> Proc (Node -> Node) -> Node
def update node, path, fn
  if path.length == 0 then
    fn.(node)
  else
    choose, *rest = path
    node.updated(
      nil,    # Don't change node type
      node.children.map do |c|
        choose.(c) ? update(c, rest, fn) : c
      end)
  end
end

# zoom_in :: Node -> [Choice] -> Node
def zoom_in node, path
  if path.length == 0 then
    node
  else
    choose, *rest = path
    chosen = node.children.select(&choose).first
    zoom_in chosen, rest
  end
end

# by_both
#   :: Proc (Node -> Bool)
#   -> Proc (Node -> Bool)
#   -> Proc (Node -> Bool)
def by_both p, q
  -> (n) { p.(n) && q.(n) }
end

# by_msg :: String -> Proc (Node -> Bool)
def by_msg msg
  -> (n) { n.children[1] == msg.to_sym }
end

# by_type :: String -> Proc (Node -> Bool)
def by_type type
  -> (n) {
    n &&
    n.is_a?(AST::Node) &&
    n.type == type.to_sym
  }
end

main if __FILE__ == $0
