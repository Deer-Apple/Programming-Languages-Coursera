# class A
#     attr_accessor :x
#     def m1
#       @x = 4
#     end
#     def m2
#       m1
#       @x > 4
#     end
#     def m3
#       @x = 4
#       @x > 4
#     end
#     def m4
#       self.x = 4
#       @x > 4
#     end
# end

# class B < A
#     def x= n
#         @x = 5
#     end
# end

# # puts B.new.m4

# class MyRange
#     include Enumerable
#     def initialize(low,high)
#       @low = low
#       @high = high
#     end
#     def each
#       i=@low
#       while i <= @high
#         yield i
#         i=i+1
#       end
#     end
#   end

# puts MyRange.new(4,2).any? {|i| i <= 4}

class A
    def initialize a
      @arr = a
    end
    def get i
      @arr[i]
    end
    def sum
      @arr.inject(0) {|acc,x| acc + x}
    end
  end
  
  class B < A
    def initialize a
      super
      @ans = false
    end
    def sum
      if !@ans
        @ans = @arr.inject(0) {|acc,x| acc + x}
      end
      @ans
    end
  end

  v = [4,19,74]
  a = A.new v
  b = B.new v
  s1 = a.sum
  puts s1
  s2 = b.sum
  puts s2
  s3 = a.sum
  puts s3
  s4 = b.sum
  puts s4