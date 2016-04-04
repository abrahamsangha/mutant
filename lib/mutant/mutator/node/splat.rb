module Mutant
  class Mutator
    class Node
      # Mutator for splat nodes
      class Splat < self

        handle :splat

        children :expression

      private

        # Perform dispatch
        #
        # @return [undefined]
        def dispatch
          emit(expression)

          return if parent_type.equal?(:mlhs)

          emit_singletons
          emit_expression_mutations
        end

      end # Splat
    end # Node
  end # Mutator
end # Mutant
