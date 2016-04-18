module Mutant
  class Mutator
    class Node
      class Literal < self
        # Mutator for regexp literals
        class Regex < self

          handle(:regexp)

          children(:body, :regopt)

          # No input can ever be matched with this
          NULL_REGEXP_SOURCE = 'nomatch\A'.freeze

        private

          # Original regexp options
          #
          # @return [Parser::AST::Node]
          def options
            children.last
          end

          # Emit mutants
          #
          # @return [undefined]
          def dispatch
            if n_str?(body)
              regexp_source =
                if body.children.none?
                  ''
                else
                  body.children.first
                end

              ast = RegularExpression::Parser.parse(regexp_source)

              REGISTRY.call(ast, self).each do |mutation|
                emit_body(s(:str, mutation.to_s))
              end
            end

            emit_singletons unless parent_node
            children.each_with_index do |child, index|
              mutate_child(index) unless n_str?(child)
            end
            emit_type(options)
            emit_type(s(:str, NULL_REGEXP_SOURCE), options)
          end

        end # Regex
      end # Literal
    end # Node
  end # Mutator
end # Mutant
