module Mutant
  module AST
    module Regexp
      module Transformer
        def self.to_ast(input)
          ExpressionMapper.call(input)
        end

        def self.to_expression(input)
          method_name = :"on_#{input.type}"

          unless ASTMapper.respond_to?(:"on_#{input.type}")
            fail "Expected to find method ASTMapper.#{method_name}\n" \
              "Unable to map node %p" % input
          end
          ASTMapper.public_send(method_name, input)
        end

        module ExpressionMapper
          PREFIX = :regexp

          extend Mutant::AST::Sexp, self

          def call(input)
            if input.quantified?
              public_send(:"on_#{input.quantifier.mode}_#{input.quantifier.token}_quantifier", input)
            else
              public_send(:"on_#{input.token}_#{input.type}", input)
            end
          end

          def on_character_set(node)
            reg(:character_set, *node.members)
          end

          def on_options_group(node)
            flags = node.options.map { |flag, enabled| reg(:flag, flag, enabled) }
            children = node.expressions.map(&method(:call))

            reg(:options_group, *flags, *children)
          end

          def on_literal_literal(node)
            reg(:literal_literal, node.text)
          end

          def on_comment_group(node)
            reg(:comment_group, node.text)
          end

          def on_literal_escape(node)
            reg(:literal_escape, node.text)
          end

          def on_named_group(node)
            reg(:named_group, node.text)
          end

          def on_number_backref(node)
            reg(:number_backref, node.text)
          end

          def on_name_call_backref(node)
            reg(:name_call_backref, node.text)
          end

          def on_hex_escape(node)
            reg(:hex_escape, node.text)
          end

          def on_codepoint_list_escape(node)
            reg(:codepoint_list_escape, node.text)
          end

          def on_root_expression(node)
            recurse(:root_expression, node)
          end

          def on_capture_group(node)
            recurse(:capture_group, node)
          end

          def on_atomic_group(node)
            recurse(:atomic_group, node)
          end

          def on_alternation_meta(node)
            recurse(:alternation_meta, node)
          end

          def on_sequence_expression(node)
            recurse(:sequence_expression, node)
          end

          def on_nlookahead_assertion(node)
            recurse(:nlookahead_assertion, node)
          end

          def on_nlookbehind_assertion(node)
            recurse(:nlookbehind_assertion, node)
          end

          def on_lookbehind_assertion(node)
            recurse(:lookbehind_assertion, node)
          end

          def on_lookahead_assertion(node)
            recurse(:lookahead_assertion, node)
          end

          def on_bos_anchor(_)
            reg(:bos_anchor)
          end

          def on_bol_anchor(_)
            reg(:bol_anchor)
          end

          def on_eol_anchor(_)
            reg(:eol_anchor)
          end

          def on_eos_anchor(_)
            reg(:eos_anchor)
          end

          def on_eos_ob_eol_anchor(_)
            reg(:eos_ob_eol_anchor)
          end

          def on_alpha_property(_)
            reg(:alpha_property)
          end

          def on_script_arabic_property(_)
            reg(:script_arabic_property)
          end

          def on_script_hangul_property(_)
            reg(:script_hangul_property)
          end

          def on_script_han_property(_)
            reg(:script_han_property)
          end

          def on_script_hiragana_property(_)
            reg(:script_hiragana_property)
          end

          def on_script_katakana_property(_)
            reg(:script_katakana_property)
          end

          def on_letter_any_property(_)
            reg(:letter_any_property)
          end

          def on_carriage_escape(_)
            reg(:carriage_escape)
          end

          def on_digit_type(_)
            reg(:digit_type)
          end

          def on_nondigit_type(_)
            reg(:nondigit_type)
          end

          def on_nonspace_type(_)
            reg(:nonspace_type)
          end

          def on_nonword_type(_)
            reg(:nonword_type)
          end

          def on_space_type(_)
            reg(:space_type)
          end

          def on_word_type(_)
            reg(:word_type)
          end

          def on_dot_escape(_)
            reg(:dot_escape)
          end

          def on_eol_escape(_)
            reg(:eol_escape)
          end

          def on_group_close_escape(_)
            reg(:group_close_escape)
          end

          def on_group_open_escape(_)
            reg(:group_open_escape)
          end

          def on_interval_close_escape(_)
            reg(:interval_close_escape)
          end

          def on_interval_open_escape(_)
            reg(:interval_open_escape)
          end

          def on_newline_escape(_)
            reg(:newline_escape)
          end

          def on_one_or_more_escape(_)
            reg(:one_or_more_escape)
          end

          def on_set_close_escape(_)
            reg(:set_close_escape)
          end

          def on_set_open_escape(_)
            reg(:set_open_escape)
          end

          def on_zero_or_more_escape(_)
            reg(:zero_or_more_escape)
          end

          def on_word_boundary_anchor(_)
            reg(:word_boundary_anchor)
          end

          def on_nonword_boundary_anchor(_)
            reg(:nonword_boundary_anchor)
          end

          def on_match_start_anchor(_)
            reg(:match_start_anchor)
          end

          def on_whitespace_free_space(_)
            reg(:whitespace_free_space)
          end

          def on_passive_group(_)
            reg(:passive_group)
          end

          def on_dot_meta(_)
            reg(:dot_meta)
          end

          def on_greedy_interval_quantifier(node)
            interval(node)
          end

          def on_reluctant_interval_quantifier(node)
            interval(node)
          end

          def on_possessive_interval_quantifier(node)
            interval(node)
          end

          def on_greedy_zero_or_more_quantifier(node)
            quantifier(node)
          end

          def on_reluctant_zero_or_more_quantifier(node)
            quantifier(node)
          end

          def on_greedy_one_or_more_quantifier(node)
            quantifier(node)
          end

          def on_reluctant_one_or_more_quantifier(node)
            quantifier(node)
          end

          def on_greedy_zero_or_one_quantifier(node)
            quantifier(node)
          end

          def on_possessive_zero_or_one_quantifier(node)
            quantifier(node)
          end

          def on_possessive_zero_or_more_quantifier(node)
            quantifier(node)
          end

          def on_possessive_one_or_more_quantifier(node)
            quantifier(node)
          end

          private

          def recurse(name, node)
            reg(name, *node.expressions.map(&method(:call)))
          end

          def quantifier(node)
            reg(
              :"#{node.quantifier.mode}_#{node.quantifier.token}",
              public_send(:"on_#{node.token}_#{node.type}", node)
            )
          end

          def interval(node)
            interval = node.quantifier

            reg(
              :"#{interval.mode}_interval",
              reg(:interval, interval.min, interval.max),
              public_send(:"on_#{node.token}_#{node.type}", node)
            )
          end

          def reg(type, *children)
            ::Parser::AST::Node.new(:"#{PREFIX}_#{type}", children)
          end
        end

        module ASTMapper
          Exp = ::Regexp::Expression
          private_constant :Exp

          MAPPING = [
            [:root,             :expression, Exp::Root                                ],
            [:literal,          :literal,    Exp::Literal                             ],
            [:dot,              :meta,       Exp::CharacterType::Any                  ],
            [:character,        :set,        Exp::CharacterSet                        ],
            [:eol,              :anchor,     Exp::Anchor::EndOfLine                   ],
            [:capture,          :group,      Exp::Group::Capture                      ],
            [:options,          :group,      Exp::Group::Options                      ],
            [:whitespace,       :free_space, Exp::WhiteSpace                          ],
            [:eos_ob_eol,       :anchor,     Exp::Anchor::EndOfStringOrBeforeEndOfLine],
            [:eos,              :anchor,     Exp::Anchor::EndOfString                 ],
            [:bol,              :anchor,     Exp::Anchor::BeginningOfLine             ],
            [:named,            :group,      Exp::Group::Named                        ],
            [:digit,            :type,       Exp::CharacterType::Digit                ],
            [:number,           :backref,    Exp::Backreference::Number               ],
            [:nlookahead,       :assertion,  Exp::Assertion::NegativeLookahead        ],
            [:alternation,      :meta,       Exp::Alternation                         ],
            [:sequence,         :expression, Exp::Alternative                         ],
            [:passive,          :group,      Exp::Group::Passive                      ],
            [:nlookbehind,      :assertion,  Exp::Assertion::NegativeLookbehind       ],
            [:word_boundary,    :anchor,     Exp::Anchor::WordBoundary                ],
            [:lookbehind,       :assertion,  Exp::Assertion::Lookbehind               ],
            [:name_call,        :backref,    Exp::Backreference::NameCall             ],
            [:word,             :type,       Exp::CharacterType::Word                 ],
            [:lookahead,        :assertion,  Exp::Assertion::Lookahead                ],
            [:atomic,           :group,      Exp::Group::Atomic                       ],
            [:bos,              :anchor,     Exp::Anchor::BeginningOfString           ],
            [:newline,          :escape,     Exp::EscapeSequence::Newline             ],
            [:space,            :type,       Exp::CharacterType::Space                ],
            [:nonspace,         :type,       Exp::CharacterType::NonSpace             ],
            [:zero_or_more,     :escape,     Exp::EscapeSequence::Literal             ],
            [:carriage,         :escape,     Exp::EscapeSequence::Return              ],
            [:dot,              :escape,     Exp::EscapeSequence::Literal             ],
            [:match_start,      :anchor,     Exp::Anchor::MatchStart                  ],
            [:set_open,         :escape,     Exp::EscapeSequence::Literal             ],
            [:set_close,        :escape,     Exp::EscapeSequence::Literal             ],
            [:literal,          :escape,     Exp::EscapeSequence::Literal             ],
            [:one_or_more,      :escape,     Exp::EscapeSequence::Literal             ],
            [:group_open,       :escape,     Exp::EscapeSequence::Literal             ],
            [:group_close,      :escape,     Exp::EscapeSequence::Literal             ],
            [:interval_open,    :escape,     Exp::EscapeSequence::Literal             ],
            [:interval_close,   :escape,     Exp::EscapeSequence::Literal             ],
            [:hex,              :escape,     Exp::EscapeSequence::Literal             ],
            [:codepoint_list,   :escape,     Exp::EscapeSequence::Literal             ],
            [:nondigit,         :type,       Exp::CharacterType::NonDigit             ],
            [:nonword,          :type,       Exp::CharacterType::NonWord              ],
            [:alpha,            :property,   Exp::UnicodeProperty::Alpha              ],
            [:script_arabic,    :property,   Exp::UnicodeProperty::Script             ],
            [:script_hangul,    :property,   Exp::UnicodeProperty::Script             ],
            [:script_han,       :property,   Exp::UnicodeProperty::Script             ],
            [:script_hiragana,  :property,   Exp::UnicodeProperty::Script             ],
            [:script_katakana,  :property,   Exp::UnicodeProperty::Script             ],
            [:letter_any,       :property,   Exp::UnicodeProperty::Letter::Any        ],
            [:comment,          :group,      Exp::Group::Comment                      ],
            [:nonword_boundary, :anchor,     Exp::Anchor::NonWordBoundary             ],
            [:eol,              :escape,     Exp::EscapeSequence::Literal             ]
          ]

          extend self

          def on_regexp_greedy_zero_or_more(ast)
            quantifier(ast, :zero_or_more, '*', 0, -1, :greedy)
          end

          def on_regexp_greedy_one_or_more(ast)
            quantifier(ast, :one_or_more, '+', 1, -1, :greedy)
          end

          def on_regexp_greedy_zero_or_one(ast)
            quantifier(ast, :zero_or_one, '?', 0, 1, :greedy)
          end

          def on_regexp_possessive_zero_or_one(ast)
            quantifier(ast, :zero_or_one, '?+', 0, 1, :possessive)
          end

          def on_regexp_reluctant_zero_or_more(ast)
            quantifier(ast, :zero_or_more, '*?', 0, -1, :reluctant)
          end

          def on_regexp_reluctant_one_or_more(ast)
            quantifier(ast, :one_or_more, '+?', 1, -1, :reluctant)
          end

          def on_regexp_possessive_zero_or_more(ast)
            quantifier(ast, :zero_or_more, '*+', 0, -1, :possessive)
          end

          def on_regexp_possessive_one_or_more(ast)
            quantifier(ast, :one_or_more, '++', 1, -1, :possessive)
          end

          def on_regexp_greedy_interval(ast)
            fail "Expected two children" unless ast.children.size.equal?(2)

            interval, subject_ast = *ast
            min, max = *interval
            subject = Transformer.to_expression(subject_ast)

            text = interval_text(min, max)

            subject.quantify(:interval, text, min, max, :greedy)

            subject
          end

          def on_regexp_reluctant_interval(ast)
            fail "Expected two children" unless ast.children.size.equal?(2)

            interval, subject_ast = *ast
            min, max = *interval
            subject = Transformer.to_expression(subject_ast)

            text = interval_text(min, max)

            subject.quantify(:interval, "#{text}?", min, max, :reluctant)

            subject
          end

          def on_regexp_possessive_interval(ast)
            fail "Expected two children" unless ast.children.size.equal?(2)

            interval, subject_ast = *ast
            min, max = *interval
            subject = Transformer.to_expression(subject_ast)

            text = interval_text(min, max)

            subject.quantify(:interval, "#{text}+", min, max, :possessive)

            subject
          end

          def on_regexp_codepoint_list_escape(ast)
            exp(:codepoint_list, :escape, ast.children.first)
          end

          def on_regexp_dot_meta(ast)
            exp(:dot, :meta, '.')
          end

          def on_regexp_character_set(ast)
            node = exp(:character, :set, '[')
            node.members = ast.children
            node
          end

          def on_regexp_eol_anchor(ast)
            exp(:eol, :anchor, '$')
          end

          def on_regexp_options_group(ast)
            flags =
              ast
                .children
                .select { |child| child.type == :regexp_flag }
                .map(&:to_a)
                .to_h

            flag_text = flags.map { |key, value| key if value }.compact.join

            node = exp(:options, :group, "(?#{flag_text}-:")
            node.options = flags

            recurse(node, ast)
          end

          def on_regexp_flag(_); end

          def on_regexp_whitespace_free_space(ast)
            exp(:whitespace, :free_space, " ")
          end

          def on_regexp_eos_ob_eol_anchor(ast)
            exp(:eos_ob_eol, :anchor, '\\Z')
          end

          def on_regexp_eos_anchor(ast)
            exp(:eos, :anchor, '\\z')
          end

          def on_regexp_bol_anchor(ast)
            exp(:bol, :anchor, '^')
          end

          def on_regexp_named_group(ast)
            exp(:named, :group, ast.children.first)
          end

          def on_regexp_digit_type(ast)
            exp(:digit, :type, '\d')
          end

          def on_regexp_number_backref(ast)
            exp(:number, :backref, ast.children.first)
          end

          def on_regexp_nlookahead_assertion(ast)
            recurse(exp(:nlookahead, :assertion, '(?!'), ast)
          end

          def on_regexp_alternation_meta(ast)
            node = exp(:alternation, :meta, '|')

            ast
              .children
              .map(&Transformer.public_method(:to_expression))
              .compact
              .each(&node.method(:alternative))

            node
          end

          def on_regexp_sequence_expression(ast)
            recurse(Exp::Alternative.new(0, 0, 0), ast)
          end

          def on_regexp_passive_group(ast)
            recurse(exp(:passive, :group, '(?:'), ast)
          end

          def on_regexp_nlookbehind_assertion(ast)
            recurse(exp(:nlookbehind, :assertion, '(?<!'), ast)
          end

          def on_regexp_word_boundary_anchor(ast)
            exp(:word_boundary, :anchor, '\b')
          end

          def on_regexp_lookbehind_assertion(ast)
            recurse(exp(:lookbehind, :assertion, '(?<='), ast)
          end

          def on_regexp_name_call_backref(ast)
            exp(:name_call, :backref, ast.children.first)
          end

          def on_regexp_word_type(ast)
            exp(:word, :type, '\w')
          end

          def on_regexp_lookahead_assertion(ast)
            recurse(exp(:lookahead, :assertion, '(?='), ast)
          end

          def on_regexp_atomic_group(ast)
            recurse(exp(:atomic, :group, '(?>'), ast)
          end

          def on_regexp_bos_anchor(ast)
            exp(:bos, :anchor, '\\A')
          end

          def on_regexp_newline_escape(ast)
            exp(:newline, :escape, '\n')
          end

          def on_regexp_space_type(ast)
            exp(:space, :type, '\s')
          end

          def on_regexp_nonspace_type(ast)
            exp(:nonspace, :type, '\S')
          end

          def on_regexp_zero_or_more_escape(ast)
            exp(:zero_or_more, :escape, '\*')
          end

          def on_regexp_carriage_escape(ast)
            exp(:carriage, :escape, '\r')
          end

          def on_regexp_dot_escape(ast)
            exp(:dot, :escape, '\.')
          end

          def on_regexp_match_start_anchor(ast)
            exp(:match_start, :anchor, '\\G')
          end

          def on_regexp_set_open_escape(ast)
            exp(:set_open, :escape, '\[')
          end

          def on_regexp_set_close_escape(ast)
            exp(:set_close, :escape, '\]')
          end

          def on_regexp_literal_escape(ast)
            exp(:literal, :escape, ast.children.first)
          end

          def on_regexp_one_or_more_escape(ast)
            exp(:one_or_more, :escape, '\+')
          end

          def on_regexp_group_open_escape(ast)
            exp(:group_open, :escape, '\(')
          end

          def on_regexp_group_close_escape(ast)
            exp(:group_close, :escape, '\)')
          end

          def on_regexp_interval_open_escape(ast)
            exp(:interval_open, :escape, '\{')
          end

          def on_regexp_interval_close_escape(ast)
            exp(:interval_close, :escape, '\}')
          end

          def on_regexp_hex_escape(ast)
            exp(:hex, :escape, ast.children.first)
          end

          def on_regexp_nondigit_type(ast)
            exp(:nondigit, :type, '\D')
          end

          def on_regexp_nonword_type(ast)
            exp(:nonword, :type, '\W')
          end

          def on_regexp_alpha_property(ast)
            exp(:alpha, :property, '\p{Alpha}')
          end

          def on_regexp_script_arabic_property(ast)
            exp(:script_arabic, :property, '\p{Arabic}')
          end

          def on_regexp_script_hangul_property(ast)
            exp(:script_hangul, :property, '\p{Hangul}')
          end

          def on_regexp_script_han_property(ast)
            exp(:script_han, :property, '\p{Han}')
          end

          def on_regexp_script_hiragana_property(ast)
            exp(:script_hiragana, :property, '\p{Hiragana}')
          end

          def on_regexp_script_katakana_property(ast)
            exp(:script_katakana, :property, '\p{Katakana}')
          end

          def on_regexp_letter_any_property(ast)
            exp(:letter_any, :property, '\p{L}')
          end

          def on_regexp_comment_group(ast)
            exp(:comment, :group, ast.children.first)
          end

          def on_regexp_nonword_boundary_anchor(ast)
            exp(:nonword_boundary, :anchor, '\\B')
          end

          def on_regexp_eol_escape(ast)
            exp(:eol, :escape, '\$')
          end

          def on_regexp_capture_group(ast)
            # node = Exp::Group::Capture.new(tok(:group, :capture, '('))
            recurse(exp(:capture, :group, '('), ast)
          end

          def on_regexp_root_expression(ast)
            recurse(Exp::Root.new, ast)
          end

          def on_regexp_literal_literal(ast)
            fail "More than one child for a literal_literal?" unless ast.children.one?

            exp(:literal, :literal, ast.children.first)
          end

          private

          def quantifier(ast, *quantifiers)
            fail "Expected one child" unless ast.children.one?

            subject = Transformer.to_expression(ast.children.first)
            subject.quantify(*quantifiers)

            subject
          end

          def interval_text(min, max)
            if min.equal?(max)
              "{#{min}}"
            elsif min.positive? && max.positive?
              "{#{min},#{max}}"
            elsif min.positive? && max.equal?(-1)
              "{#{min},}"
            elsif min.equal?(0) && max.positive?
              "{,#{max}}"
            end
          end

          def recurse(expression, ast)
            unless expression.is_a?(::Regexp::Expression::Base)
              fail "Expected `expression` to be Regexp::Expression"
                "but got: #{expression.inspect} (#{expression.class})"
            end

            unless ast.is_a?(Parser::AST::Node)
              fail "Expected `ast` to be Parser::AST::Node but got: #{ast.inspect} (#{ast.class})"
            end

            ast
              .children
              .map(&Transformer.public_method(:to_expression))
              .compact
              .each(&expression.method(:<<))

            expression
          end

          def exp(token_name, type_name, *arguments)
            no_match = lambda { fail "Could not find mapping for (#{token_name}, #{type_name})" }

            _, _, klass =
              MAPPING.detect(no_match) do |(token, type, _)|
                token.equal?(token_name) && type.equal?(type_name)
              end

            klass.new(tok(type_name, token_name, *arguments))
          end

          def tok(*arguments)
            ::Regexp::Token.new(*arguments)
          end
        end
      end
    end
  end # AST
end # Mutant
