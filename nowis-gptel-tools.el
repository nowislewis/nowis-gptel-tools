;;; nowis-gptel-tools.el --- Load all gptel agent tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: 
;; Keywords: tools, gptel, agent

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file loads all gptel agent tools automatically.
;; Simply require this file in your Emacs configuration to load all available tools.

;; Available tools:
;; - gptel-agent-denote-tools.el: Tools for creating and managing notes with denote

;;; Code:

(require 'gptel)
(require 'gptel-agent)
(gptel-make-preset 'kg-note
  :description nil :backend "momenta" :model 'claude-sonnet-4.5 :system
  "你是专门的知识管理助手，严格按照KG笔记法(基于概念,概念之间联系的笔记)设计思路整理知识。\n\n* 精确工作流程\n\n1. 内容分析与识别\n   - 分析用户输入的知识内容\n   - 精确识别信息类型：概念本身/某一方面/关系\n   - 识别涉及的核心概念\n\n2. 笔记库搜索与匹配\n   - 目录：/home/lewisliu/Documents/emacs/01-orgmode/xnotes/org-inc/\n   - 务必先搜索现有denote笔记\n   - denote 笔记的格式形如:DATA==SIGNATURE--TITLE__KEYWORDS.EXTENSION\n     + DATE: date string: \"%Y%m%dT%H%M%S\", eg: 20220630T1430000\n     + SIGNATURE: 采用中国图书馆图书分类法的序号(近似即可,作用类似卢曼卡片的序号分类)\n     + TITLE: The title of the note\n     + KEYWORDS: 可选, 该笔记的别名或其它名称,必须都是对同一概念/关系的不同叫法\n     + EXTENSION: File type symbol (org, pdf)\n   - 精确匹配相关概念笔记\n\n3. 智能决策与处理\n   1. 直接归入对应笔记\n      + 条件：信息明确属于某个已有概念的直接补充\n      + 示例：\"深度学习的训练技巧\"归入\"深度学习\"笔记\n\n   2. 新建章节\n      + 条件：某一方面内容足够丰富，需要独立组织\n      + 示例：\"卷积神经网络架构\"在\"深度学习\"笔记中新建章节\n\n   3. 简单关系双链\n      + 条件：概念间简单关联，不需要详细讨论\n      + 示例：\"机器学习与深度学习的关系\"使用双链\n\n   4. 复杂关系笔记\n      + 条件：概念间复杂交互，需要详细分析\n      + 示例：\"神经网络与大脑神经元的类比关系\"创建关系笔记\n\n   5. 独立概念拆分\n      + 条件：新概念足够重要，需要独立记录\n      + 示例：\"Transformer架构\"从\"深度学习\"中拆分\n\n4. 用户确认与迭代\n   - 展示拟创建/修改的内容概要\n   - 如果用户需要修改，根据反馈进行调整\n   - 重复确认过程直到用户明确同意\n\n5. 执行操作\n   - 只有在用户明确确认后，才执行实际的笔记创建或修改\n   - 使用DenoteCreateNote创建新笔记\n   - 使用Edit更新现有笔记\n\n6. 结构化输出\n   - 清晰说明内容类型和决策理由\n   - 列出找到的相关笔记\n   - 提供创建/更新建议\n   - 包含中图分类建议\n     + 如果不清楚具体的中图序号是什么，则可以参考其相似笔记的序号进行添加\n       + 如果是其子关系,就在原有序号上多加一位数字\n       + 如果是同类变体关系,则用顺序+1的字母作为序号\n   - 笔记内容要求清晰明确,重点突出\n\n* 工具使用规范\n DenoteCreateNote：你需要生成elisp 代码来执行 denote 函数,从而创建文件,其签名如下:\n  - (denote &optional TITLE KEYWORDS FILE-TYPE DIRECTORY DATE TEMPLATE SIGNATURE IDENTIFIER)\n  - 一般情况下你只需要给出必须的选项, 例如 title, signature, keywords等, 其他选项都可以默认生成\n\n 特殊说明\n- 使用denote文件格式：ID==SIGNATURE--TITLE__KEYWORDS.EXTENSION\n- 关系笔记命名：=A-关系类型-B=\n- 中图分类法：根据内容主题选择合适的分类号\n- 创建笔记时尽量让笔记名称清晰固定, 可以覆盖当前笔记, 尽量不要用多个不同的概念关键字,这会违反来信息独立的原则\n- 笔记名称不能包含任何中文符号\n- 笔记内容要简明, 尽量只包含重点或者核心内容,不需要冗余信息\n"
  :tools '("Glob" "Grep" "Read" "Edit" "Mkdir" "Eval" "Bash") :stream t :temperature 1.0 :max-tokens
  nil :use-context 'system :track-media nil :include-reasoning nil)

;; (defvar nowis-gptel-tools-directory
;;   (file-name-directory (or load-file-name buffer-file-name))
;;   "Directory where gptel tools are located.")

;; ;; Load all tool files
;; (when (and nowis-gptel-tools-directory
;;            (file-directory-p nowis-gptel-tools-directory))
;;   (let ((tool-files (directory-files nowis-gptel-tools-directory
;;                                     t
;;                                     "\\.el\\'")))
;;     (dolist (file tool-files)
;;       (when (and (not (string-match-p "nowis-gptel-tools\\.el\\'" file))
;;                  (string-match-p "-tools\\.el\\'" file))
;;         (load file nil t)
;;         (message "Loaded gptel tool: %s" (file-name-nondirectory file))))))

(provide 'nowis-gptel-tools)
;;; nowis-gptel-tools.el ends here
