
require 'parser/current'
require 'unparser'

Parser::Builders::Default.emit_lambda = true
Parser::Builders::Default.emit_procarg0 = true

module FormulaManip

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

end
