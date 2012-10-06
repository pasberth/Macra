describe "Macra Parser" do

  describe "Assign" do

    example do
      `./bin/macra --nodes "!assign x y"`.should == "(Assign (Sym x) (Sym y))"
    end

    example do
      `./bin/macra --nodes "!assign x 1"`.should == "(Assign (Sym x) (Int 1))"
    end
  end

  describe "Lambda expression" do

    example do
      `./bin/macra --nodes "!lambda x y"`.should == "(Lambda (Sym x) (Sym y))"
    end
  end

  describe "If cond then" do

    example do
      `./bin/macra --nodes "!if cond then"`.should == "(If (Sym cond) (Sym then))"
    end
  end

  describe "Funcall" do

    example do
      `./bin/macra --nodes "!funcall x y"`.should == "(Funcall (Sym x) (Sym y))"
    end

    example do
      `./bin/macra --nodes "!funcall !funcall x y z"`.should == "(Funcall (Funcall (Sym x) (Sym y)) (Funcall (Sym z)))"
    end

    example do
      `./bin/macra --nodes "!funcall x !funcall y z"`.should == "(Funcall (Sym x) (Funcall (Sym y) (Sym z)))"
    end
  end

  describe "Infix operator" do

    example do
      `./bin/macra --nodes "then :if cond"`.should == "(Maccall (Sym :if) [Sym then, Sym cond])"
    end

    example do
      `./bin/macra --nodes "a :+ b"`.should == "(Maccall (Sym :+) [Sym a, Sym b])"
    end

    example do
      `./bin/macra --nodes "map x => x :* x"`.should == "(Funcall (Sym :map) (Maccall (Sym :+) [Sym then, Sym cond]))"
    end
  end
end
