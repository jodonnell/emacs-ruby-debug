class TestClass
  def initialize integer
    @integer = integer
    power_house
  end

  def power_house
    @integer *= 2
    @integer.to_s
  end
end
