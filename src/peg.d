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
    @safe Value!V get(V)() const {
        return cast(Value!V) value;
    }

    /// check value type.
    @safe nothrow bool isValueType(V)() const {
        return cast(Value!V) value;
    }
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
    }

    /// save current position.
    @property @safe nothrow Position save() const {
        return Position(source_.save, line_, position_);
    }

    /// restore position.
    @safe nothrow void restore(ref const Position pos) {
        source_ = pos.source;
        line_ = pos.line;
        position_ = pos.position;
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

private:

    /// parsing source.
    R source_;

    /// current line number. (start from 1)
    size_t line_ = 1;

    /// current position.
    size_t position_ = 0;
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
            return memo.result;
        }
        
        const before = ctx.save;
        auto r = Result!R(false, before.source, before.source);

        try {
            r.match = parse(ctx, r.value);
            const after = ctx.save;
            r.end = after.source;
            memo_[before.position] = MemoEntry(r, before, after);
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

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new SequenceParser!Range(
        new CharParser!(Range, 't'),
        new CharParser!(Range, 'e'));

    auto r = p(c);
    assert(r.match);
    assert(r.begin == src);
    assert(r.end == src[2..$]);
    assert(c.position == 2);
    assert(c.front == 's');

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

static assert(static_test!({
    auto src = "test";
    alias typeof(src) Range;

    auto c = makeContext(src);
    auto p = new ChoiceParser!Range(
        new CharParser!(Range, 't'),
        new CharParser!(Range, 'e'));

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

/// main function.
int main(string[] args) {
    return 0;
}
