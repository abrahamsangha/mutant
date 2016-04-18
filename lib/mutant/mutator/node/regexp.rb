module Mutant
  class Mutator
    class Node

      # Generic mutator
      module RegularExpression
        class Parser
          def self.parse(source)
            Node.new(Regexp::Parser.parse(source, 'ruby/2.3'))
          end

          class Node < DelegateClass(Regexp::Expression::Base)
            def to_source
              to_s
            end

            def with(expressions)
              new_node =
                dup.tap do |dup_node|
                  dup_node.expressions = expressions
                end

              self.class.new(new_node)
            end

            def type
              :"regexp_#{super()}"
            end

            def children
              return [] unless respond_to?(:expressions)
              expressions.map(&self.class.public_method(:new))
            end
          end
        end

        class Node < Node

        private

          # Emit mutations
          #
          # @return [undefined]
          def dispatch
            children.each_with_index do |child, index|
              REGISTRY.call(child, self).each do |mutation|
                new_children = children.dup
                new_children[index] = mutation
                emit(node.with(new_children).to_source)
              end
            end
          end

          class Generic < self
            handle(
              :regexp_set,
              :regexp_anchor,
              :regexp_backref,
              :regexp_assertion,
              :regexp_subset,
              :regexp_conditional,
              :regexp_type,
              :regexp_keep,
              :regexp_quantifier,
              :regexp_free_space,
              :regexp_property,
              :regexp_escape
            )

            def dispatch; end
          end

          class Expression < self
            handle(:regexp_expression)
          end

          class Literal < self
            handle(:regexp_literal)
          end

          class Group < self
            handle(:regexp_group)
          end

          class Meta < self
            handle(:regexp_meta)

          private

            def dispatch
              super()

              children.each_index do |index|
                dup_children = children.dup
                dup_children.delete_at(index)
                emit(node.with(dup_children))
              end
            end
          end
        end
      end
    end # Node
  end # Mutator
end # Mutant
