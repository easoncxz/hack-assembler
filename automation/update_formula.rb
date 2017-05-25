
require 'parser/ruby21'
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

  # Insert or replace the bottle for a given OS
  # put_bottle :: String -> String -> Node -> Node
  def put_bottle os, sha256, klass
    update(
      klass,
      bot_begin_path,
      put_bottle_version(os, sha256))
  end

  # Path to the :begin node
  # bot_begin_path :: [Choice]
  # type Choice = Proc (Node -> Bool)
  def bot_begin_path
    [ by_type('begin'),
      by_both(
        by_type('block'),
        by_child(
          by_both(
            by_type('send'),
            by_msg('bottle')))),
      by_type('begin')]
  end

  # Tricky: this is an insert-or-update
  # put_bottle_version :: String -> String -> Proc (Node -> Node)
  def put_bottle_version os, sha256
    -> (bot_begin) {
      bot_begin.updated(
        nil,  # keep the node type the unchanged
        bot_begin.children.reject(
          # Get rid of any existing matching ones
          &by_both(
            by_msg('sha256'),
            by_os(os))
        # Then add the one we want
        ).push(new_sha256(sha256, os)))
    }
  end

  # Build a new AST Node
  # String -> String -> Node
  def new_sha256 sha256, os
    # Unparser doesn't like Sexp, so let's bring
    # own own bit of "source code" inline.
    sha256_send = Parser::CurrentRuby.parse(
      'sha256 "checksum-here" => :some_os')
    with_sha256 = update(
      sha256_send,
      [ by_type('hash'),
        by_type('pair'),
        by_type('str') ],
      -> (n) { n.updated(nil, [sha256]) })
    with_sha256_and_os = update(
      with_sha256,
      [ by_type('hash'),
        by_type('pair'),
        by_type('sym') ],
      -> (n) { n.updated(nil, [os.to_sym]) })
    with_sha256_and_os
  end

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

  # Matches if one of the node's children matches the given p
  # by_child :: Proc (Node -> Bool) -> Proc (Node -> Bool)
  def by_child p
    -> (n) {
      n &&
      n.is_a?(AST::Node) &&
      n.children.select(&p).size > 0
    }
  end

  # Matches if this :send node expresses the give sha256 sum
  # by_os :: String -> Proc (Node -> Bool)
  def by_os os
    -> (n) {
      zoom_in(n, [
        by_type('hash'),
        by_type('pair'),
        by_type('sym')])
      .children[0] == os.to_sym
    }
  end

end
