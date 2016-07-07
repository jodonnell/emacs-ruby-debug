#!/usr/bin/env ruby

require 'byebug'
require_relative './test_class'

apple = 1 + 2
byebug
apple = apple * 2
obj = TestClass.new 3
obj.power_house

puts 'Complet' + 'ed 200'
