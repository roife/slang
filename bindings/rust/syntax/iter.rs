use crate::{SyntaxCursor, SyntaxElement, SyntaxNode};

pub struct SyntaxIdxChildren<'a> {
    parent: SyntaxNode<'a>,
    start_idx: usize,
    end_idx: usize,
}

impl<'a> SyntaxIdxChildren<'a> {
    pub fn new(parent: SyntaxNode<'a>) -> Self {
        SyntaxIdxChildren {
            parent,
            start_idx: 0,
            end_idx: parent.child_count(),
        }
    }
}

impl<'a> Iterator for SyntaxIdxChildren<'a> {
    type Item = (usize, SyntaxElement<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        (self.start_idx..self.end_idx).find_map(|idx| {
            self.parent.child(idx).map(|child| {
                self.start_idx = idx + 1;
                (idx, child)
            })
        })
    }
}

impl<'a> DoubleEndedIterator for SyntaxIdxChildren<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        (self.start_idx..self.end_idx).rev().find_map(|idx| {
            self.parent.child(idx).map(|child| {
                self.end_idx = idx;
                (idx, child)
            })
        })
    }
}

impl<'a> ExactSizeIterator for SyntaxIdxChildren<'a> {
    fn len(&self) -> usize {
        self.end_idx - self.start_idx
    }
}

pub struct SyntaxChildren<'a>(SyntaxIdxChildren<'a>);

impl<'a> SyntaxChildren<'a> {
    pub fn new(parent: SyntaxNode<'a>) -> Self {
        SyntaxChildren(SyntaxIdxChildren::new(parent))
    }
}

impl<'a> Iterator for SyntaxChildren<'a> {
    type Item = SyntaxElement<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, elem)| elem)
    }
}

impl<'a> DoubleEndedIterator for SyntaxChildren<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|(_, elem)| elem)
    }
}

impl<'a> ExactSizeIterator for SyntaxChildren<'a> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

pub struct SyntaxAncestors<'a> {
    node: Option<SyntaxNode<'a>>,
}

impl<'a> SyntaxAncestors<'a> {
    pub fn new(node: SyntaxNode<'a>) -> Self {
        SyntaxAncestors { node: Some(node) }
    }
}

impl<'a> Iterator for SyntaxAncestors<'a> {
    type Item = SyntaxNode<'a>;

    fn next(&mut self) -> Option<SyntaxNode<'a>> {
        let res = self.node.take()?;
        self.node = res.parent();
        Some(res)
    }
}

pub struct SyntaxPreorder<'a> {
    cursor: SyntaxCursor<'a>,
    done: bool,
}

impl<'a> SyntaxPreorder<'a> {
    pub fn new(root: SyntaxNode<'a>) -> Self {
        let cursor = SyntaxCursor::new(root);
        SyntaxPreorder {
            cursor,
            done: false,
        }
    }
}

impl<'a> Iterator for SyntaxPreorder<'a> {
    type Item = SyntaxElement<'a>;

    fn next(&mut self) -> Option<SyntaxElement<'a>> {
        if self.done {
            return None;
        }

        let res = self.cursor.to_elem();

        let cursor = &mut self.cursor;
        if cursor.goto_first_child() {
            return Some(res);
        }

        while !cursor.goto_next_sibling() {
            if !cursor.goto_parent() || cursor.is_root() {
                self.done = true;
                break;
            }
        }
        Some(res)
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use itertools::Itertools;

    use crate::{SyntaxAncestors, SyntaxPreorder, SyntaxTree};

    use super::SyntaxIdxChildren;

    fn get_test_tree() -> SyntaxTree {
        SyntaxTree::from_text("module A(input a); wire x; endmodule;", "source", "")
    }

    #[test]
    fn test_syntax_preorder() {
        let tree = get_test_tree();
        let root = tree.root().unwrap();

        let ans = SyntaxPreorder::new(root)
            .map(|elem| format!("{:?}", elem.kind()))
            .join("\n");
        let expected = expect![[r#"
            Node(CompilationUnit)
            Node(SyntaxList)
            Node(ModuleDeclaration)
            Node(SyntaxList)
            Node(ModuleHeader)
            Token(ModuleKeyword)
            Token(Unknown)
            Token(Identifier)
            Node(SyntaxList)
            Node(AnsiPortList)
            Token(OpenParenthesis)
            Node(SeparatedList)
            Node(ImplicitAnsiPort)
            Node(SyntaxList)
            Node(VariablePortHeader)
            Token(Unknown)
            Token(InputKeyword)
            Token(Unknown)
            Node(ImplicitType)
            Token(Unknown)
            Node(SyntaxList)
            Token(Placeholder)
            Node(Declarator)
            Token(Identifier)
            Node(SyntaxList)
            Token(CloseParenthesis)
            Token(Semicolon)
            Node(SyntaxList)
            Node(NetDeclaration)
            Node(SyntaxList)
            Token(WireKeyword)
            Token(Unknown)
            Node(ImplicitType)
            Token(Unknown)
            Node(SyntaxList)
            Token(Placeholder)
            Node(SeparatedList)
            Node(Declarator)
            Token(Identifier)
            Node(SyntaxList)
            Token(Semicolon)
            Token(EndModuleKeyword)
            Node(EmptyMember)
            Node(SyntaxList)
            Node(TokenList)
            Token(Semicolon)
            Token(EndOfFile)"#]];
        expected.assert_eq(&ans);
    }

    #[test]
    fn test_syntax_children() {
        let tree = get_test_tree();
        let root = tree.root().unwrap();
        let node = root.child_node(0).unwrap();
        let node = node.child_node(0).unwrap();
        let node = node.child_node(1).unwrap();

        {
            let ans = SyntaxIdxChildren::new(node)
                .map(|(idx, elem)| format!("{idx}: {:?}", elem.kind()))
                .join("\n");
            let expected = expect![[r#"
                0: Token(ModuleKeyword)
                1: Token(Unknown)
                2: Token(Identifier)
                3: Node(SyntaxList)
                5: Node(AnsiPortList)
                6: Token(Semicolon)"#]];
            expected.assert_eq(&ans);
        }

        {
            let ans = SyntaxIdxChildren::new(node)
                .rev()
                .map(|(idx, elem)| format!("{idx}: {:?}", elem.kind()))
                .join("\n");
            let expected = expect![[r#"
                6: Token(Semicolon)
                5: Node(AnsiPortList)
                3: Node(SyntaxList)
                2: Token(Identifier)
                1: Token(Unknown)
                0: Token(ModuleKeyword)"#]];
            expected.assert_eq(&ans);
        }
    }

    #[test]
    fn test_syntax_ancestor() {
        let tree = get_test_tree();
        let root = tree.root().unwrap();
        let node = root.child_node(0).unwrap();
        let node = node.child_node(0).unwrap();
        let node = node.child_node(1).unwrap();

        let ans = SyntaxAncestors::new(node)
            .map(|elem| format!("{:?}", elem.kind()))
            .join("\n");
        let expected = expect![[r#"
            ModuleHeader
            ModuleDeclaration
            CompilationUnit"#]];
        expected.assert_eq(&ans);
    }
}
