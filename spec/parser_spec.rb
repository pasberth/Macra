describe "Macra Parser" do

  describe "Bracket" do
    example do
      `./bin/macra --nodes '(a; b; c)'`.should == <<-A
#maccall
  #maccall
    #maccall
      '(
      'a
    'b
  'c
A
    end
  end

  describe "Assign" do

    example do
      `./bin/macra --nodes "!define x y"`.should == <<-A
!define
  'x
  'y
A
    end

    example do
      `./bin/macra --nodes "!define x 1"`.should == <<-A
!define
  'x
  1.0
A
    end
  end

  describe "Lambda expression" do

    example do
      `./bin/macra --nodes "!lambda x y"`.should == <<-A
!lambda
  'x
  'y
A
    end
  end

  describe "If cond then" do

    example do
      `./bin/macra --nodes "!if cond then"`.should == <<-A
!if
  'cond
  'then
A
    end
  end

  describe "Funcall" do

    example do
      `./bin/macra --nodes "!funcall x y"`.should == <<-A
!funcall
  'x
  'y
A
    end

    example do
      `./bin/macra --nodes "!funcall !funcall x y z"`.should == <<-A
!funcall
  !funcall
    'x
    'y
  'z
A
    end

    example do
      `./bin/macra --nodes "!funcall x !funcall y z"`.should == <<-A
!funcall
  'x
  !funcall
    'y
    'z
A
    end
  end

  describe "Infix operator" do

    example do
      `./bin/macra --nodes "then :if cond"`.should == <<-A
#maccall
  #maccall
    'if
    'then
  'cond
A
    end

    example do
      `./bin/macra --nodes "a :+ b"`.should == <<-A
#maccall
  #maccall
    '+
    'a
  'b
A

    end

    example do
      `./bin/macra --nodes "map x => x :* x"`.should == <<-A
#maccall
  'map
  #maccall
    #maccall
      '=>
      'x
    #maccall
      #maccall
        '*
        'x
      'x
A
    end
  end

  describe "Suffix operator" do
    example do
      `./bin/macra --nodes "a b @isNull"`.should == <<-A
#maccall
  'isNull
  #maccall
    'a
    'b
A
    end
  end
end
