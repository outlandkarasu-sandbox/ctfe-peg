/**
 *  It is compile time PEG compiler compiler.
 *  generate parser from PEG source in compile time.
 *  generated parser can run at compile time, too.
 *
 *  Copyright: Copyright Outlandish Watch 2007 - 2012
 *  Authors: outland.karasu at gmail.com
 *  Version: $Header$
 *  License: <a href="http://www.boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
 */

module peg;

import std.stdio;
import std.range;
import std.traits;

/// static unit test template.
private template static_test(alias T) {
    enum static_test = ({
        T();
        return true;
    })();
}

/**
 *  semantic value class.
 *
 *  Params:
 *      V = value type.
 */
class Value(V) {
public:

    /// initialize with value.
    this(V value) {this.value = value;}

    /// inner value.
    V value;

    /// alias this.
    alias value this;
}

/**
 *  parsing result.
 *
 *  Params:
 *      R = source range type.
 */
struct Result(R) {
    bool match;
    R begin;
    R end;
    Object value;

    /// get semantic value.
    @safe const(V) get(V)() const {
        return (cast(const(Value!V))value).value;
    }

    /// check value type.
    @safe nothrow bool isValueType(V)() const {
        return cast(const(Value!V)) value;
    }
}

/**
 *  AST node type.
 *
 *  Params:
 *      R = source range type.
 */
class Node(R) {
public:

    /// initialize id and children.
    this(string id, ref const Result!R result, Node!R[] children) {
        id_ = id;
        result_ = result;
        children_ = children;
    }

    /// return node id.
    @property @safe nothrow pure string id() const {return id_;}

    /// return result.
    @property @safe nothrow pure
    ref const(Result!R) result() const {return result_;}

    /// return children node.
    @property @safe nothrow pure
    const(Node!R[]) children() const {return children_;}

private:

    /// node ID.
    string id_;

    /// matching result.
    const(Result!R) result_;

    /// children node.
    const(Node!R[]) children_;
}

/**
 *  parsing context class.
 *
 *  Params:
 *      R = source range type.
 */
class Context(R) if(isForwardRange!R) {
public:

    /// initialize with source.
    @safe this(R src) {
        source_ = src.save;
    }

    /// parsing position. for backtrack.
    struct Position {
        R source;
        size_t line;
        size_t position;
        size_t nodeCount;
    }

    /// save current position.
    @property @safe nothrow Position save() const {
        return Position(source_.save, line_, position_, nodes_.length);
    }

    /// restore position.
    @safe nothrow void restore(ref const Position pos) {
        source_ = pos.source;
        line_ = pos.line;
        position_ = pos.position;
        nodes_.length = pos.nodeCount;
    }

    /// return current position.
    @property @safe nothrow size_t position() const {return position_;}

    /// return current line.
    @property @safe nothrow size_t line() const {return line_;}

    @safe nothrow void addLine() {++line_;}

    /// return is empty.
    @property @safe nothrow bool empty() const {return source_.empty;}

    /// return current element.
    @property @safe auto front() const {return source_.front;}

    /// pop element and advance position.
    @safe void popFront() {
        source_.popFront();
        ++position_;
    }

    /// add AST node.
    @safe void addNode(
            size_t begin, string id, ref const Result!R result) {
        if(begin < nodes_.length) {
            auto node = new Node!R(id, result, nodes_[begin..$].dup);
            nodes_ = nodes_[0..begin] ~ node;
        } else {
            auto node = new Node!R(id, result, null);
            nodes_ ~= node;
        }
    }

    /// ditto
    @safe void addNodes(Node!R[] nodes) {
        if(nodes.length > 0) {
            nodes_ ~= nodes;
        }
    }

    /// get AST nodes.
    @safe Node!R[] getNodes(size_t begin = 0) {
        return (begin < nodes_.length) ? nodes_[begin..$].dup : null;
    }

    /// return current node count.
    @property @safe nothrow size_t nodeCount() const {return nodes_.length;}

private:

    /// parsing source.
    R source_;

    /// current line number. (start from 1)
    size_t line_ = 1;

    /// current position.
    size_t position_ = 0;

    /// nodes.
    Node!R[] nodes_;
}

/**
 *  make parsing context.
 *
 *  Params:
 *      R = source range type.
 *      src = source.
 */
Context!R makeContext(R)(R src) {return new Context!R(src);}

static assert(static_test!({
    auto c = makeContext("test");
    assert(c);

    assert(c.position == 0);
    assert(c.line == 1);
    assert(c.front == 't');

    auto pos = c.save;

    c.popFront();

    assert(c.position == 1);
    assert(c.line == 1);
    assert(c.front == 'e');

    c.addLine();

    assert(c.position == 1);
    assert(c.line == 2);
    assert(c.front == 'e');

    c.restore(pos);

    assert(c.position == 0);
    assert(c.line == 1);
    assert(c.front == 't');
}));

/**
 *  abstract parser class.
 *
 *  Params:
 *      R = source range type.
 */
abstract class Parser(R) if(isForwardRange!R) {
public:

    /**
     *  parsing source.
     *
     *  Params:
     *      ctx = parsing context.
     *  Returns:
     *      parsed result.
     */
    @safe Result!R opCall(Context!R ctx) {
        if(auto memo = ctx.position in memo_) {
            ctx.restore(memo.end);
            ctx.addNodes(memo.nodes);
            return memo.result;
        }
        
        const before = ctx.save;
        auto r = Result!R(false, before.source, before.source);

        try {
            r.match = parse(ctx, r.value);
            const after = ctx.save;
            r.end = after.source;
            auto nodes = ctx.getNodes(before.nodeCount);
            memo_[before.position] = MemoEntry(r, before, after, nodes);
        } finally {
            // restore if unmatched or throws exception.
            if(!r.match) {
                ctx.restore(before);
            }
        }

        return r;
    }

    /**
     *  parsing implementation.
     *  require override by derived classes.
     *
     *  not require backtracking on unmatched.
     *  implemented outer function.
     *
     *  Params:
     *      ctx = parsing context.
     *      value = output semantic value.
     *  Returns:
     *      return true if matched.
     */
    protected abstract @safe bool parse(Context!R ctx, out Object value);

private:

    /// memo entry.
    struct MemoEntry {
        Result!R result;
        Context!R.Position begin;
        Context!R.Position end;
        Node!R[] nodes;
    }

    /// memoize data.
    MemoEntry[size_t] memo_;
}

/// parse any char class.
class AnyCharParser(R) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        if(!ctx.empty) {
            ctx.popFront();
            return true;
        }
        return false;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new AnyCharParser!Range;
    auto r = p(c);

    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[1 .. $]);

    assert(c.position == 1);
    assert(c.front == 'e');

    r = p (c);
    assert(r.match);

    r = p(c);
    assert(r.match);

    r = p(c);
    assert(r.match);

    r = p(c);
    assert(!r.match);
    assert(c.position == 4);
    assert(c.empty);
}));

/// parse a char class.
class CharParser(R, alias C) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        if(!ctx.empty && ctx.front == C) {
            ctx.popFront();
            return true;
        }
        return false;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new CharParser!(Range, 't');
    auto r = p(c);

    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[1 .. $]);

    assert(c.position == 1);
    assert(c.front == 'e');

    r = p(c);
    assert(!r.match);
    assert(c.position == 1);
    assert(!c.empty);
}));

/// match to end of source.
class EndParser(R) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        return ctx.empty;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new AnyCharParser!Range;
    auto p2 = new EndParser!Range;

    assert(!p2(c).match);
    auto r = p(c);
    assert(r.match);

    assert(!p2(c).match);
    r = p(c);
    assert(r.match);

    assert(!p2(c).match);
    r = p(c);
    assert(r.match);

    assert(!p2(c).match);
    r = p(c);
    assert(r.match);

    r = p(c);
    assert(!r.match);
    assert(c.position == 4);
    assert(c.empty);
    assert(p2(c).match);
}));

/// parse a string class.
class StringParser(R, alias S) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        foreach(c; S) {
            if(!ctx.empty && ctx.front == c) {
                ctx.popFront();
            } else {
                return false;
            }
        }
        return true;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new StringParser!(Range, "te");
    auto r = p(c);

    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[2 .. $]);

    assert(c.position == 2);
    assert(c.front == 's');

    r = p(c);
    assert(!r.match);
    assert(c.position == 2);
    assert(!c.empty);
}));

/// parse a char in char set.
class CharSetParser(R, alias S) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        if(!ctx.empty) {
            const front = ctx.front;
            foreach(c; S) {
                if(front == c) {
                    ctx.popFront();
                    return true;
                }
            }
        }
        return false;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new CharSetParser!(Range, "stuv");
    auto r = p(c);

    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[1 .. $]);

    assert(c.position == 1);
    assert(c.front == 'e');

    r = p(c);
    assert(!r.match);
    assert(c.position == 1);
    assert(!c.empty);
}));

/// parse a char in char range.
class CharRangeParser(R, alias B, alias E) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        if(!ctx.empty) {
            const front = ctx.front;
            if(B <= front && front <= E) {
                ctx.popFront();
                return true;
            }
        }
        return false;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new CharRangeParser!(Range, 's', 'z');
    auto r = p(c);

    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[1 .. $]);

    assert(c.position == 1);
    assert(c.front == 'e');

    r = p(c);
    assert(!r.match);
    assert(c.position == 1);
    assert(!c.empty);
}));

/// non terminal symbole parser. it has a inner parser.
abstract class NonTerminalParser(R) : Parser!R {
    /// initialize with inner parser.
    public @safe nothrow this(Parser!R parser) {
        parser_ = parser;
    }

protected:

    @property @safe nothrow Parser!R parser() {return parser_;}

private:

    /// inner parser.
    Parser!R parser_;
}

/// AND predicator.
class AndPredParser(R) : NonTerminalParser!R {
    /// initialize with inner parser.
    public @safe nothrow this(Parser!R parser) {super(parser);}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        const pos = ctx.save;
        const r = parser()(ctx);
        ctx.restore(pos);
        return r.match;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new AndPredParser!Range(new CharParser!(Range, 't'));

    auto r = p(c);
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src);
    assert(c.position == 0);
    assert(c.front == 't');

    c.popFront();
    r = p(c);
    assert(!r.match);
    assert(c.position == 1);
    assert(!c.empty);
}));

/// NOT predicator.
class NotPredParser(R) : NonTerminalParser!R {
    /// initialize with inner parser.
    public @safe nothrow this(Parser!R parser) {super(parser);}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        const pos = ctx.save;
        const r = parser()(ctx);
        ctx.restore(pos);
        return !r.match;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new NotPredParser!Range(new CharParser!(Range, 't'));

    auto r = p(c);
    assert(!r.match);
    assert(r.begin == src);
    assert(r.end == src);
    assert(c.position == 0);
    assert(c.front == 't');

    c.popFront();
    r = p(c);
    assert(r.match);
    assert(c.position == 1);
    assert(!c.empty);
}));

/// repeat zero or more.
class ZeroOrMoreRepeatParser(R) : NonTerminalParser!R {
    /// initialize with inner parser.
    public @safe nothrow this(Parser!R parser) {super(parser);}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        auto p = parser();
        while(p(ctx).match) {}
        return true;
    }
}

static assert(static_test!({
    auto src = "ttst";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new ZeroOrMoreRepeatParser!Range(new CharParser!(Range, 't'));

    auto r = p(c);
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[2..$]);
    assert(c.position == 2);
    assert(c.front == 's');

    r = p(c);
    assert(r.match);
    assert(c.position == 2);

    c.popFront();
    r = p(c);
    assert(r.match);
    assert(c.position == 4);
}));

/// repeat one or more.
class OneOrMoreRepeatParser(R) : NonTerminalParser!R {
    /// initialize with inner parser.
    public @safe nothrow this(Parser!R parser) {super(parser);}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        auto p = parser();
        if(!p(ctx).match) {
            return false;
        }
        while(p(ctx).match) {}
        return true;
    }
}

static assert(static_test!({
    auto src = "ttst";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new OneOrMoreRepeatParser!Range(new CharParser!(Range, 't'));

    auto r = p(c);
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[2..$]);
    assert(c.position == 2);
    assert(c.front == 's');

    r = p(c);
    assert(!r.match);
    assert(c.position == 2);

    c.popFront();
    r = p(c);
    assert(r.match);
    assert(c.position == 4);
}));

/// non terminal symbole parser. it has inner parsers.
abstract class NonTerminalArrayParser(R) : Parser!R {
    /// initialize with inner parsers.
    public @safe nothrow this(Parser!R[] parsers ...) {
        parsers_ = parsers.dup;
    }

protected:

    @property @safe nothrow Parser!R[] parsers() {return parsers_;}

private:

    /// inner parsers.
    Parser!R[] parsers_;
}

/// parser sequence.
class SequenceParser(R) : NonTerminalArrayParser!R {

    /// initialize with inner parsers.
    public @safe nothrow this(Parser!R[] parsers ...) {super(parsers);}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        foreach(p; parsers) {
            if(!p(ctx).match) {
                return false;
            }
        }
        return true;
    }
}

/// make sequence parser.
SequenceParser!R seq(R)(Parser!R[] parsers ...) {
    return new SequenceParser!R(parsers);
}

static assert(static_test!({
    auto src = "tets";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = seq(new CharParser!(Range, 't'), new CharParser!(Range, 'e'));

    auto r = p(c);
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[2..$]);
    assert(c.position == 2);
    assert(c.front == 't');

    r = p(c);
    assert(!r.match);
    assert(c.position == 2);
}));
	
/// orderd choice parser.
class ChoiceParser(R) : NonTerminalArrayParser!R {

    /// initialize with inner parsers.
    public @safe nothrow this(Parser!R[] parsers ...) {super(parsers);}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        foreach(p; parsers) {
            if(p(ctx).match) {
                return true;
            }
        }
        return false;
    }
}

/// make sequence parser.
ChoiceParser!R choice(R)(Parser!R[] parsers ...) {
    return new ChoiceParser!R(parsers);
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = choice(new CharParser!(Range, 't'), new CharParser!(Range, 'e'));

    auto r = p(c);
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[1..$]);
    assert(c.position == 1);
    assert(c.front == 'e');

    r = p(c);
    assert(r.match);
    assert(r.begin == src[1..$]);
    assert(r.end == src[2..$]);
    assert(c.position == 2);
    assert(c.front == 's');

    r = p(c);
    assert(!r.match);
    assert(c.position == 2);
}));

/// rule parser.
class RuleParser(R) : Parser!R {
    /// set other parser.
    public @property @safe nothrow
    void parser(Parser!R p) {parser_ = p;}

    /// get inner parser.
    public @property @safe nothrow
    Parser!R parser() {return parser_;}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        return parser_(ctx).match;
    }

    /// inner parser.
    private Parser!R parser_;
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new RuleParser!Range;
    p.parser = new AnyCharParser!Range;
    auto r = p(c);
    assert(c.position == 1);
    assert(c.front == 'e');
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[1 .. $]);

    r = p(c);
    assert(r.match);
    assert(c.position == 2);

    r = p(c);
    assert(r.match);
    assert(c.position == 3);

    r = p(c);
    assert(r.match);
    assert(c.position == 4);

    r = p(c);
    assert(!r.match);
    assert(c.position == 4);
    assert(c.empty);
}));

/// node parser.
class NodeParser(R, alias ID) : NonTerminalParser!R {
    /// initialize with inner parser.
    public @safe this(Parser!R parser) {super(parser);}

    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        auto cnt = ctx.nodeCount;
        auto r = parser()(ctx);
        if(r.match) {
            ctx.addNode(cnt, ID, r);
        }
        return r.match;
    }
}

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new NodeParser!(Range, "TEST_ID")(new AnyCharParser!Range);

    auto r = p(c);
    assert(c.position == 1);
    assert(c.front == 'e');
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[1 .. $]);

    auto nodes = c.getNodes();
    assert(nodes.length == 1);
    assert(nodes[0].id == "TEST_ID");
    assert(nodes[0].result == r);
}));

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto makeParser = {return new NodeParser!(Range, "TEST_ID")(seq(
        new NodeParser!(Range, "TEST_ID1")(new AnyCharParser!Range),
        new NodeParser!(Range, "TEST_ID2")(new AnyCharParser!Range),
        new NodeParser!(Range, "TEST_ID3")(new AnyCharParser!Range)
    ));};
    auto p = makeParser();

    auto r = p(c);
    assert(c.position == 3);
    assert(c.front == 't');
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[3 .. $]);

    auto nodes = c.getNodes();
    assert(nodes.length == 1);
    assert(nodes[0].id == "TEST_ID");
    assert(nodes[0].result == r);

    auto children = nodes[0].children;
    assert(children.length == 3);
    assert(children[0].id == "TEST_ID1");
    assert(children[1].id == "TEST_ID2");
    assert(children[2].id == "TEST_ID3");

    c = makeContext("teste");
    p = makeParser();
    r = p(c);
    assert(r.match);

    r = p(c);
    assert(c.nodeCount == 1);
}));

/// hex character to uint.
private @safe pure nothrow uint hex2uint(dchar c) {
    switch(c) {
    case '0': .. case '9':
        return c - '0';
    case 'a': .. case 'f':
        return 10 + (c - 'a');
    case 'A': .. case 'F':
        return 10 + (c - 'A');
    default:
        assert(false, "not hex char.");
        return 0;
    }
}

/// check a char.
private @safe bool checkChar(R)(R ctx, dchar c) {
    return !ctx.empty && ctx.front == c;
}

/**
 *  read a char literal from source.
 *  it can read a single char or escape sequence.
 *
 *  Params:
 *      R = source range type.
 *      ctx = source context.
 *      c = output char.
 *  Returns:
 *      true if succeeded read a char.
 */
private @safe bool parsePegCharLiteral(R)(R ctx, out dchar c) {
    if(ctx.empty) {
        return false;
    }

    immutable c1 = ctx.front;
    ctx.popFront();
    if(c1 == '\\') {
        // read escape sequence.
        if(ctx.empty) {
            return false;
        }
        immutable c2 = ctx.front;
        switch(c2) {
        case '\'': c = '\''; ctx.popFront(); break;
        case '\"': c = '\"'; ctx.popFront(); break;
        case '\?': c = '\?'; ctx.popFront(); break;
        case '\\': c = '\\'; ctx.popFront(); break;
        case 'a': c = '\a'; ctx.popFront(); break;
        case 'b': c = '\b'; ctx.popFront(); break;
        case 'f': c = '\f'; ctx.popFront(); break;
        case 'n': c = '\n'; ctx.popFront(); break;
        case 'r': c = '\r'; ctx.popFront(); break;
        case 't': c = '\t'; ctx.popFront(); break;
        case 'v': c = '\v'; ctx.popFront(); break;
        case '0': .. case '7':
            c = '\0';
            do {
                c <<= 3;
                c |= ctx.front - '0';
                ctx.popFront();
            } while(!ctx.empty && ('0' <= ctx.front && ctx.front <= '7'));
            break;
        case 'x', 'X': {
            ctx.popFront();

            if(ctx.empty) {
                return false;
            }
            auto x1 = ctx.front;
            ctx.popFront();

            if(ctx.empty) {
                return false;
            }
            auto x2 = ctx.front;
            ctx.popFront();

            c = cast(char)((hex2uint(x1) << 4) | hex2uint(x2));
            break;
        }
        default:
            break;
        }
    } else {
        // read normal character.
        switch(c1) {
        case '\r', '\n', '\0':
            // abnormal character.
            return false;
        default:
            c = c1;
            break;
        }
    }
    return true;
}

static assert(static_test!({
    void testCharLiteral(string s, dchar c) {
        auto ctx = makeContext(s);
        dchar ch;
        assert(parsePegCharLiteral(ctx, ch));
        assert(c == ch);
        assert(ctx.empty);
    }

    void testNotCharLiteral(string s) {
        auto ctx = makeContext(s);
        dchar ch;
        assert(!parsePegCharLiteral(ctx, ch));
    }

    testCharLiteral("t", 't');
    testCharLiteral("\\t", '\t');
    testCharLiteral("\\\'", '\'');
    testCharLiteral("\\\"", '\"');
    testCharLiteral("\\\?", '\?');
    testCharLiteral("\\\\", '\\');
    testCharLiteral("\\a", '\a');
    testCharLiteral("\\b", '\b');
    testCharLiteral("\\f", '\f');
    testCharLiteral("\\n", '\n');
    testCharLiteral("\\r", '\r');
    testCharLiteral("\\t", '\t');
    testCharLiteral("\\v", '\v');
    testCharLiteral("\\0", '\0');
    testCharLiteral("\\123", '\123');
    testCharLiteral("\\x00", '\x00');
    testCharLiteral("\\xFF", '\xFF');
    testCharLiteral("\\xaa", '\xaa');

    testNotCharLiteral("\r");
    testNotCharLiteral("\n");
    testNotCharLiteral("\0");
}));

/// character literal parser
class PegCharLiteralParser(R) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        if(!checkChar(ctx, '\'')) {
            return false;
        }
        ctx.popFront();

        if(checkChar(ctx, '\'')) {
            return false;
        }

        dchar c;
        if(!parsePegCharLiteral(ctx, c)) {
            return false;
        }

        if(!checkChar(ctx, '\'')) {
            return false;
        }
        ctx.popFront();

        value = new Value!dchar(c);
        return true;
    }
}

static assert(static_test!({
    void testPegCharLiteral(string s, dchar c) {
        auto ctx = makeContext(s);
        auto p = new PegCharLiteralParser!string;
        auto r = p(ctx);
        assert(r.match);
        assert(r.begin == s);
        assert(r.get!(dchar)() == c);
        assert(ctx.empty);
    }

    void testNotPegCharLiteral(string s) {
        auto ctx = makeContext(s);
        auto p = new PegCharLiteralParser!string;
        auto r = p(ctx);
        assert(!r.match);
        assert(r.value is null);
        assert(ctx.position == 0);
    }

    testPegCharLiteral("'a'", 'a');
    testPegCharLiteral("'\\a'", '\a');
    testPegCharLiteral("'\\0'", '\0');
    testPegCharLiteral("'\\x12'", '\x12');
    testPegCharLiteral("'\\xFf'", '\xFF');

    testNotPegCharLiteral("\"a\"");
    testNotPegCharLiteral("a'");
    testNotPegCharLiteral("'a");
    testNotPegCharLiteral("''");
}));

/// string literal parser
class PegStringLiteralParser(R) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        if(ctx.empty || ctx.front != '\"') {
            return false;
        }
        ctx.popFront();

        string buf;
        for(dchar c;
                !ctx.empty &&
                ctx.front != '\"' &&
                parsePegCharLiteral(ctx, c);) {

            // if c is 1 byte character, put single byte char into buffer.
            // (not extend multi bytes)
            // buf c is over 1 byte range,
            // put multi bytes char into buffer.
            if(c <= 0xFFU) {
                buf ~= cast(char) c;
            } else {
                buf ~= c;
            }
        }

        if(ctx.empty || ctx.front != '\"') {
            return false;
        }
        ctx.popFront();

        value = new Value!string(buf);
        return true;
    }
}

static assert(static_test!({
    void testPegStringLiteral(string s, string ex) {
        auto ctx = makeContext(s);
        auto p = new PegStringLiteralParser!string;
        auto r = p(ctx);
        assert(r.match);
        assert(r.begin == s);
        assert(r.get!string().length == ex.length, r.get!string());
        assert(r.get!string() == ex, r.get!string());
        assert(ctx.empty);
    }

    void testNotPegStringLiteral(string s) {
        auto ctx = makeContext(s);
        auto p = new PegStringLiteralParser!string;
        auto r = p(ctx);
        assert(!r.match);
        assert(r.value is null);
        assert(ctx.position == 0);
    }

    testPegStringLiteral(q{"test"}, "test");
    testPegStringLiteral(q{"\"\""}, "\"\"");
    testPegStringLiteral(q{"\""}, "\"");
    testPegStringLiteral(q{"\0\0"}, "\0\0");
    testPegStringLiteral(q{"あいう"}, "あいう");
    testPegStringLiteral(q{"\xFF\xaa"}, "\xFF\xAA");
    testPegStringLiteral(q{"01234"}, "01234");

    testNotPegStringLiteral("\"test");
    testNotPegStringLiteral("'test'");
    testNotPegStringLiteral("test\"");
    testNotPegStringLiteral("\"");
}));

/// parse identifier head char.
private @safe bool checkPegIdentifierHead(R)(R ctx) {
    if(ctx.empty) {
        return false;
    }
    switch(ctx.front) {
    case 'a': .. case 'z':
    case 'A': .. case 'Z':
    case '_':
        return true;
    default:
        return false;
    }
}

/// parse identifier tail char.
private @safe bool checkPegIdentifierTail(R)(R ctx) {
    if(ctx.empty) {
        return false;
    }
    switch(ctx.front) {
    case '0': .. case '9':
    case 'a': .. case 'z':
    case 'A': .. case 'Z':
    case '_':
        return true;
    default:
        return false;
    }
}

/// PEG identifier parser
class PegIdentifierParser(R) : Parser!R {
    // parse implements.
    protected @safe bool parse(Context!R ctx, out Object value) {
        if(!checkPegIdentifierHead(ctx)) {
            return false;
        }

        string buf;
        buf ~= ctx.front;
        ctx.popFront();

        for(;checkPegIdentifierTail(ctx);
                buf ~= ctx.front, ctx.popFront()) {}
        value = new Value!string(buf);
        return true;
    }
}

static assert(static_test!({
    void testPegIdentifier(string s, string ex) {
        auto ctx = makeContext(s);
        auto p = new PegIdentifierParser!string;
        auto r = p(ctx);
        assert(r.match);
        assert(r.begin == s);
        assert(r.get!string().length == ex.length, r.get!string());
        assert(r.get!string() == ex, r.get!string());
        assert(ctx.empty);
    }

    void testNotPegIdentifier(string s) {
        auto ctx = makeContext(s);
        auto p = new PegIdentifierParser!string;
        auto r = p(ctx);
        assert(!r.match);
        assert(r.value is null);
        assert(ctx.position == 0);
    }

    testPegIdentifier("test", "test");
    testPegIdentifier("test1234", "test1234");
    testPegIdentifier("t123", "t123");
    testPegIdentifier("t", "t");

    testNotPegIdentifier("");
    testNotPegIdentifier("1");
    testNotPegIdentifier("1test");
}));

/// main function.
int main(string[] args) {
    return 0;
}
