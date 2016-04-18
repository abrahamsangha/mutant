Mutant::Meta::Example.add :'regexp_expression' do
  source '/(foo|bar)/'

  singleton_mutations

  # match all inputs
  mutation '//'

  # match no input
  mutation '/nomatch\A/'

  mutation '/(foo)/'
  mutation '/(bar)/'
end
