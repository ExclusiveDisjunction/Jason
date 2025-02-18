//
// Created by Hollan on 12/15/24.
//

#ifndef QUICK_ASTNODE_H
#define QUICK_ASTNODE_H

#include <iostream>

class ASTNode
{
protected:
    ASTNode(std::shared_ptr<ASTNode> left, std::shared_ptr<ASTNode> right) : left(std::move(left)), right(std::move(right)) { }
public:
    std::shared_ptr<ASTNode> right = nullptr;

    std::shared_ptr<ASTNode> left = nullptr;
public:
    ASTNode() = default;
    ASTNode(const ASTNode& obj) noexcept = default;
    ASTNode(ASTNode&& obj) noexcept = default;
    virtual ~ASTNode() = default;
    
    ASTNode& operator=(const ASTNode& obj) noexcept = default;
    ASTNode& operator=(ASTNode&& obj) noexcept = default;
    
    virtual void Print(std::ostream& out) const noexcept = 0;
    [[nodiscard]] virtual unsigned GetWidth() const noexcept = 0;
    [[nodiscard]] unsigned GetTotalWidth() const noexcept
    {
        if (!left && !right)
            return this->GetWidth();
        else
        {
            if (left && !right)
                return this->left->GetTotalWidth() + this->GetWidth() + 1;
            else if (!left && right)
                return this->right->GetTotalWidth() + this->GetWidth() + 1;
            else 
                return this->left->GetTotalWidth() + 1 + this->right->GetTotalWidth() + 1 + this->GetWidth();
            
            /*
             We split this into three cases:
             Leaf
             One child
             Two children
             
             For a leaf, the total width is the width of our-self.
             For a one child, we take the width of that child, plus one (spacing), plus our-selves.
             For two children, we take the width of the left, plus one (spacing), the width of the right, plus one (spacing), plus our-selves.
             */
        }
    }
    
    [[nodiscard]] static std::shared_ptr<ASTNode> Join(std::shared_ptr<ASTNode> left, std::shared_ptr<ASTNode> right, std::shared_ptr<ASTNode> root)
    {
        root->left = std::move(left);
        root->right = std::move(right);
        return root;
    }
};

inline std::ostream& operator<<(std::ostream& out, const ASTNode& obj) noexcept
{
    obj.Print(out);
    return out;
}

#endif //QUICK_ASTNODE_H
