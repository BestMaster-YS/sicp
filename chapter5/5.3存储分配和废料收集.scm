

;; 伪代码
(
 begin-garbage-collection
 ;; 开始GC之前准备
 (assign free (const 0))
 (assign scan (const 0))
 (assign old (reg root))
 (assign relocate-continue (label reassign-root))
 (goto (label relocate-old-result-in-new))
 ;; 完成GC之后
 reassign-root
 (assign root (reg new))
 (goto (label gc-loop))
 gc-loop
 ;; 检查 free 和 scan 指针是否相同
 (test (op =) (reg free) (reg scan))
 (branch (label gc-flip))
 ;; 寻找到 scan 指向的car进行垃圾回收，后续更新该 car 对应位置的值
 (assign old (op vector-ref) (reg new-cars) (reg scan))
 (assign relocate-continue (label update-car))
 (goto (label relocate-old-result-in-new))
 update-car
 ;; relocate-old-result-in-new 会将 new 赋值为新值
 (perform (op vector-set!) (reg new-cars) (reg scan) (reg new))
 ;; 进行 scan 指针的 cdr 垃圾回收
 (assign old (op vector-ref) (reg new-cdrs) (reg scan))
 (assign relocate-continue (label update-cdr))
 (goto (label relocate-old-result-in-new))
 update-cdr
 (perform (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
 (assign scan (op +) (reg scan) (const 1))
 ;; 开始下轮的垃圾回收
 (goto (label gc-loop))
 ;; 子程序按如下方式进行重新分配：
 ;; 若要求重新分配的对象（由old指向）不是序对，子程序就返回指向该对象的同一个指针，并不做任何修改
 ;; 若是序对则执行重新分配操作，若序对包含一个破碎之心标记，则说明序对已被移走，因此提出其中的前向地址（cdr位置），并在new中返回该地址
 ;; 若old指向尚未移动的序对，则把序对移动到新存储区的第一个自由位置（free指向），将破碎之心标记和前向地址存入序对的老地址，设置好破碎之心。
 ;; relocate-old-result-in-new 用寄存器oldcr保存由old指向的对象的car或cdr
 ;; old ，new，前向地址是指向存储数组的下标
 relocate-old-result-in-new
 (test (op pointer-to-pair?) (reg old))
 (branch (label pair))
 (assign new (reg old))
 (goto (reg relocate-continue))
 pair
 (assign oldcr (op vector-ref) (reg the-cars) (reg old))
 (test (op broken-heart?) (reg oldcr))
 (branch (label already-moved))
 (assign new (reg free))
 ;; 递增free
 (assign free (op +) (reg free) (const 1))
 ;; 更新 new-cars
 (perform (op vector-set!)
          (reg new-cars) (reg new) (reg oldcr))
 (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
 ;; 更新 new-cdrs
 (perform (op vector-set!)
          (reg new-cdrs) (reg new) (reg oldcr))
 ;; 将 old 更新为 broken-heart
 (perform (op vector-set!)
          (reg the-cars) (reg old) (const broken-heart))
 (perform (op vector-set!)
          (reg the-cdrs) (reg old) (reg new))
 (goto (reg relocate-continue))
 already-moved
 (assign new (op vector-ref) (reg the-cdrs) (reg old))
 (goto (reg relocate-continue))
 ;; 废料收集最后，交换老存储区和新存储区的对象，只需交换指针的值：the-cars与new-cars呼唤，the-cdrs和new-cdrs互换
 gc-flip
 (assign temp (reg the-cars))
 (assign the-cars (reg new-cars))
 (assign new-cars (reg temp))
 (assign temp (reg the-cdrs))
 (assign the-cdrs (reg new-cdrs))
 (assign new-cdrs (reg temp))
 )



