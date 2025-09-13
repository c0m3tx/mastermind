package main

import (
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestXes(t *testing.T) {
	assert.Equal(t, int8(4), xes([]int8{1, 2, 3, 4}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(3), xes([]int8{1, 2, 3, 5}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(2), xes([]int8{2, 1, 3, 4}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(1), xes([]int8{1, 5, 6, 7}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(0), xes([]int8{5, 6, 7, 8}, []int8{1, 2, 3, 4}))
}

func TestOes(t *testing.T) {
	assert.Equal(t, int8(0), oes([]int8{1, 2, 3, 4}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(1), oes([]int8{1, 2, 4, 5}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(2), oes([]int8{2, 1, 3, 4}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(3), oes([]int8{2, 1, 4, 5}, []int8{1, 2, 3, 4}))
	assert.Equal(t, int8(4), oes([]int8{2, 1, 4, 3}, []int8{1, 2, 3, 4}))
}

func TestUserInputOk(t *testing.T) {
	reader := strings.NewReader("1234\n")
	res, err := userInput(reader)
	assert.Equal(t, []int8{1, 2, 3, 4}, res)
	assert.NoError(t, err)
}

func TestUserInputDiscardsExtraSpaces(t *testing.T) {
	reader := strings.NewReader("1 2 3 4\n")
	res, err := userInput(reader)
	assert.Equal(t, []int8{1, 2, 3, 4}, res)
	assert.NoError(t, err)
}

func TestUserInputDiscardsLetters(t *testing.T) {
	reader := strings.NewReader("1 2 3a4\n")
	res, err := userInput(reader)
	assert.Equal(t, []int8{1, 2, 3, 4}, res)
	assert.NoError(t, err)
}

func TestUserInputDisallowsTooManyDigits(t *testing.T) {
	reader := strings.NewReader("12345\n")
	_, err := userInput(reader)
	assert.Error(t, err)
	assert.Equal(t, "invalid input", err.Error())
}

func TestUserInputDisallowsTooFewDigits(t *testing.T) {
	reader := strings.NewReader("123\n")
	_, err := userInput(reader)
	assert.Error(t, err)
	assert.Equal(t, "invalid input", err.Error())
}

func TestUserInputDisallowsDups(t *testing.T) {
	reader := strings.NewReader("1233\n")
	_, err := userInput(reader)
	assert.Error(t, err)
	assert.Equal(t, "invalid input", err.Error())
}

func TestUserInputReaderIsEOF(t *testing.T) {
	reader := strings.NewReader("123")
	_, err := userInput(reader)
	assert.Error(t, err)
	assert.ErrorIs(t, err, io.EOF)
}

func TestOutput(t *testing.T) {
	type testCase struct {
		x   int8
		o   int8
		exp string
	}

	cases := []testCase{
		{3, 1, "XXXO"},
		{1, 2, "XOO"},
		{2, 0, "XX"},
		{0, 1, "O"},
		{0, 0, ""},
	}

	for _, cs := range cases {
		writer := new(strings.Builder)
		output(writer, cs.x, cs.o)
		assert.Equal(t, strings.Trim(writer.String(), "\n"), "Result: "+cs.exp)
	}
}

func TestGenerateSecret(t *testing.T) {
	for i := 0; i < 100; i++ {
		secret := generateSecret()
		assert.Len(t, secret, 4)
		assert.Len(t, dedup(secret), 4)
	}
}

func TestDedup(t *testing.T) {
	assert.EqualValues(t, dedup([]int8{1, 2, 3, 3}), []int8{1, 2, 3})
	assert.EqualValues(t, dedup([]int8{3, 2, 1, 3}), []int8{3, 2, 1})
	assert.EqualValues(t, dedup([]int8{3, 3, 3, 3}), []int8{3})

}
