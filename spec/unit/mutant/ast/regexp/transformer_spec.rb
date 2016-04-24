RSpec.describe Mutant::AST::Regexp::Transformer do
  def self.r(type, *children)
    ::Parser::AST::Node.new(:"regexp_#{type}", children)
  end

  def self.root(*children)
    r(:root_expression, *children)
  end

  TRANSFORMATIONS = {
    /foo/          => root(r(:literal_literal, 'foo')),
    /(foo)/        => root(r(:capture_group, r(:literal_literal, 'foo'))),
    /\A/           => root(r(:bos_anchor)),
    /\z/           => root(r(:eos_anchor)),
    /\z/           => root(r(:eos_anchor)),
    /\b/           => root(r(:word_boundary_anchor)),
    /\B/           => root(r(:nonword_boundary_anchor)),
    /\G/           => root(r(:match_start_anchor)),
    /\Z/           => root(r(:eos_ob_eol_anchor)),
    /$/            => root(r(:eol_anchor)),
    /^/            => root(r(:bol_anchor)),
    /.*/           => root(r(:greedy_zero_or_more, r(:dot_meta))),
    /.+/           => root(r(:greedy_one_or_more, r(:dot_meta))),
    /.?/           => root(r(:greedy_zero_or_one, r(:dot_meta))),
    /.?+/          => root(r(:possessive_zero_or_one, r(:dot_meta))),
    /.{2}/         => root(r(:greedy_interval, r(:interval, 2, 2), r(:dot_meta))),
    /.{3,5}/       => root(r(:greedy_interval, r(:interval, 3, 5), r(:dot_meta))),
    /a{1,}/        => root(r(:greedy_interval, r(:interval, 1, -1), r(:literal_literal, 'a'))),
    /.{,3}/        => root(r(:greedy_interval, r(:interval, 0, 3), r(:dot_meta))),
    /.{1,3}?/      => root(r(:reluctant_interval, r(:interval, 1, 3), r(:dot_meta))),
    /.{1,3}?/      => root(r(:reluctant_interval, r(:interval, 1, 3), r(:dot_meta))),
    /.{1,3}+/      => root(r(:possessive_interval, r(:interval, 1, 3), r(:dot_meta))),
    /.*?/          => root(r(:reluctant_zero_or_more, r(:dot_meta))),
    /.+?/          => root(r(:reluctant_one_or_more, r(:dot_meta))),
    /.*+/          => root(r(:possessive_zero_or_more, r(:dot_meta))),
    /.++/          => root(r(:possessive_one_or_more, r(:dot_meta))),
    /.?+/          => root(r(:possessive_zero_or_one, r(:dot_meta))),
    /(?>a)/        => root(r(:atomic_group, r(:literal_literal, 'a'))),
    /\r/           => root(r(:carriage_escape)),
    /\./           => root(r(:dot_escape)),
    /\$/           => root(r(:eol_escape)),
    /\)/           => root(r(:group_close_escape)),
    /\(/           => root(r(:group_open_escape)),
    /\}/           => root(r(:interval_close_escape)),
    /\{/           => root(r(:interval_open_escape)),
    /\ /           => root(r(:literal_escape, '\\ ')),
    /\#/           => root(r(:literal_escape, '\\#')),
    /\-/           => root(r(:literal_escape, '\\-')),
    /\:/           => root(r(:literal_escape, '\\:')),
    /\</           => root(r(:literal_escape, '\\<')),
    /\xFF/n        => root(r(:hex_escape, '\\xFF')),
    /\u{9879}/     => root(r(:codepoint_list_escape, '\\u{9879}')),
    /\n/           => root(r(:newline_escape)),
    /\+/           => root(r(:one_or_more_escape)),
    /\]/           => root(r(:set_close_escape)),
    /\[/           => root(r(:set_open_escape)),
    /\*/           => root(r(:zero_or_more_escape)),
    /()\1/         => root(r(:capture_group), r(:number_backref, '\1')),
    /(?<a>)\g<a>/  => root(r(:named_group, '(?<a>'), r(:name_call_backref, '\g<a>')),
    /(?#foo)/      => root(r(:comment_group, '(?#foo)')),
    /(?<foo>)/     => root(r(:named_group, '(?<foo>')),
    /(?:)/         => root(r(:passive_group)),
    /\d/           => root(r(:digit_type)),
    /\D/           => root(r(:nondigit_type)),
    /\S/           => root(r(:nonspace_type)),
    /\W/           => root(r(:nonword_type)),
    /\s/           => root(r(:space_type)),
    /\w/           => root(r(:word_type)),
    /\p{Alpha}/    => root(r(:alpha_property)),
    /\p{Alpha}/    => root(r(:alpha_property)),
    /\p{Arabic}/   => root(r(:script_arabic_property)),
    /\p{Hangul}/   => root(r(:script_hangul_property)),
    /\p{Han}/      => root(r(:script_han_property)),
    /\p{Hiragana}/ => root(r(:script_hiragana_property)),
    /\p{Katakana}/ => root(r(:script_katakana_property)),
    /\p{L}/        => root(r(:letter_any_property)),
    /[ab]/         => root(r(:character_set, 'a', 'b')),
    /(?!a)/        => root(r(:nlookahead_assertion, r(:literal_literal, 'a'))),
    /(?<!a)/       => root(r(:nlookbehind_assertion, r(:literal_literal, 'a'))),
    /(?<=a)/       => root(r(:lookbehind_assertion, r(:literal_literal, 'a'))),
    /(?=a)/        => root(r(:lookahead_assertion, r(:literal_literal, 'a'))),
    /(?x-: )/      => root(
      r(:options_group,
        r(:flag, :m, false),
        r(:flag, :i, false),
        r(:flag, :x, true),
        r(:flag, :d, false),
        r(:flag, :a, false),
        r(:flag, :u, false),
        r(:whitespace_free_space)
      )
    ),
    /foo|bar/      => root(
      r(:alternation_meta,
        r(:sequence_expression, r(:literal_literal, 'foo')),
        r(:sequence_expression, r(:literal_literal, 'bar'))
      )
    ),
  }

  def indent(string)
    string.gsub(/^/, ' ' * 6)
  end

  def assert_expressions(actual, expected)
    expect(actual.type).to  be(expected.type)
    expect(actual.token).to be(expected.token)
    expect(actual.text).to  eql(expected.text)

    if actual.quantified? != expected.quantified?
      fail "Original and generated expressions do not have the same `quantified?` value"
    end

    if actual.quantified?
      expect(actual.quantifier.token).to be(expected.quantifier.token)
      expect(actual.quantifier.text).to eql(expected.quantifier.text)
      expect(actual.quantifier.mode).to be(expected.quantifier.mode)
      expect(actual.quantifier.min).to be(expected.quantifier.min)
      expect(actual.quantifier.max).to be(expected.quantifier.max)
    end

    if actual.terminal? != expected.terminal?
      fail "Original and generated expressions do not have the same `terminal?` value"
    end

    return if actual.terminal?

    actual.expressions.zip(expected.expressions).each do |(actual_exp, expected_exp)|
      assert_expressions(actual_exp, expected_exp)
    end
  end

  TRANSFORMATIONS.each.with_index(1) do |(regex, ast), index|
    it "transforms #{regex.inspect}" do
      expression = Regexp::Parser.parse(regex)
      transform  = described_class.to_ast(expression)

      expect(transform).to eql(ast)
    end

    next if regex == /\xFF/n

    it "round trips #{regex.inspect}" do
      original   = Regexp::Parser.parse(regex)
      ast        = described_class.to_ast(original)
      expression = described_class.to_expression(ast)
      round_trip = expression.to_re

      expect_exp_text = indent(YAML.dump(Regexp::Parser.parse(regex)))
      actual_exp_text = indent(YAML.dump(described_class.to_expression(ast)))

      diff = Mutant::Diff.build(expect_exp_text, actual_exp_text).colorized_diff

      fail_message = <<-MSG
#{'Failed performing round trip:'}
  Input:

#{indent(regex.inspect)}

  Parse:

#{expect_exp_text}

  AST:

#{indent(ast.inspect)}

  Expression:

#{actual_exp_text}

  Diff:

#{indent(diff) if diff}

  Final:

#{indent(described_class.to_expression(ast).to_re.inspect)}
MSG

      expect(round_trip).to eql(regex), fail_message
      assert_expressions(expression, original)
    end
  end
end
