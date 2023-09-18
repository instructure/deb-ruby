require_relative '../../spec_helper'
require_relative 'fixtures/classes'
require_relative 'shared/strip'

describe "String#strip" do
  it_behaves_like :string_strip, :strip

  it "returns a new string with leading and trailing whitespace removed" do
    "   hello   ".strip.should == "hello"
    "   hello world   ".strip.should == "hello world"
    "\tgoodbye\r\v\n".strip.should == "goodbye"
  end

  it "returns a copy of self without leading and trailing NULL bytes and whitespace" do
    " \x00 goodbye \x00 ".strip.should == "goodbye"
  end
end

describe "String#strip!" do
  it "modifies self in place and returns self" do
    a = "   hello   "
    a.strip!.should equal(a)
    a.should == "hello"

    a = "\tgoodbye\r\v\n"
    a.strip!
    a.should == "goodbye"
  end

  it "returns nil if no modifications where made" do
    a = "hello"
    a.strip!.should == nil
    a.should == "hello"
  end

  it "makes a string empty if it is only whitespace" do
    "".strip!.should == nil
    " ".strip.should == ""
    "  ".strip.should == ""
  end

  it "removes leading and trailing NULL bytes and whitespace" do
    a = "\000 goodbye \000"
    a.strip!
    a.should == "goodbye"
  end

  it "raises a FrozenError on a frozen instance that is modified" do
    -> { "  hello  ".freeze.strip! }.should raise_error(FrozenError)
  end

  # see #1552
  it "raises a FrozenError on a frozen instance that would not be modified" do
    -> {"hello".freeze.strip! }.should raise_error(FrozenError)
    -> {"".freeze.strip!      }.should raise_error(FrozenError)
  end
end
