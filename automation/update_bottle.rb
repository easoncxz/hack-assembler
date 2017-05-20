#!/usr/bin/env ruby

require 'parser/current'
require 'unparser'

Parser::Builders::Default.emit_lambda = true
Parser::Builders::Default.emit_procarg0 = true

####

def main
  if ARGV.length == 0 then
    puts "usage: update_bottle my-formula.rb"
    exit 1
  end
  ast = read_ast ARGV[0]
end

def read_ast filepath
  sourceStr = IO.read(filepath)
  Parser::CurrentRuby.parse_with_comments(sourceStr)
end

def write_ast filepath, ast, comments
  sourceStr = Unparser.unparse(ast, comments)
  IO.write(filepath, sourceStr)
end

def ast
  a, c = read_ast "/Users/eason/pg/homebrew-tap/Formula/hack-assembler.rb"
  a
end


####

# update_formula_field :: Node -> String -> String -> Node
def update_formula_field klass, field, value
  update(
    klass,
    [ by_type('begin'),
      by_both(
        by_type('send'),
        by_msg(field)),
      by_type('str')],
    -> (n) { n.updated(nil, [value]) })
end


####


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
